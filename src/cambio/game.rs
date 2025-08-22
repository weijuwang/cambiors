use super::*;
use CannotExecuteReason::*;
use fast_jagged_vec::*;

use std::iter;
use std::ops::*;
use std::cmp::Ordering;
use std::fmt::Formatter;

/// Reasons why an action cannot be executed. This is exhaustive; every single possible reason I
/// know of has its own enum variant.
/// TODO Invalid stick error?
pub enum CannotExecuteReason {
    /// The caller claims that the card is [attested_card], but we have it as [actual_card].
    ConflictingInfo {
        actual_card: Card,
        attested_card: Card
    },

    /// We don't know a card and need it to complete the action, but the caller didn't provide it.
    NotEnoughInfo,

    /// There is no such card location in this game.
    InvalidCardLocation(CardLocation),

    /// The caller claims that the attested card was just revealed, but we know this is impossible
    /// because it isn't in the list of unseen cards. This can be a drawn card or a player's card.
    ImpossibleUnseenCard(Card),

    /// After drawing, the player tried to swap the drawn card with another player's card.
    CannotSwapWithOther(CardLocation),

    /// Peeking your own card is not allowed here, but peeking someone else's card is.
    CannotPeekOwn(CardLocation),

    /// Peeking someone else's card is not allowed here, but peeking your own card is.
    CannotPeekOther(CardLocation),

    /// This switch is not possible after drawing a black king because none of the two card locations
    /// is the card that the player just peeked at.
    InvalidBlackKingSwitch(CardLocation, CardLocation),

    /// This switch is not possible because the cards belong to the same player.
    CannotSwitchSamePlayerCards(CardLocation, CardLocation),

    /// Calling Cambio is not possible because another player already did it.
    CambioAlreadyCalled(Player),

    /// The discard pile is empty, so sticking is not possible here.
    StickOnEmptyDiscardPile(CardLocation),

    /// This action can't be executed in the current state.
    IllegalAction
}

pub type ExecuteActionResult<T> = Result<T, CannotExecuteReason>;

impl Display for CannotExecuteReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConflictingInfo { actual_card, attested_card } =>
                write!(f, "Card is known to be {actual_card} but attested as {attested_card}"),
            NotEnoughInfo =>
                write!(f, "Card wasn't given"),
            InvalidCardLocation(location) =>
                write!(f, "Invalid card location {location}"),
            ImpossibleUnseenCard(attested_card) =>
                write!(f, "Revealed card can't be {attested_card} as attested"),
            CannotSwapWithOther(location) =>
                write!(f, "Cannot swap drawn card with {location}; must swap with own card or discard"),
            CannotPeekOwn(location) =>
                write!(f, "Can't peek {location}; must peek another player's card or pass"),
            CannotPeekOther(location) =>
                write!(f, "Can't peek {location}; must peek your own card or pass"),
            InvalidBlackKingSwitch(a, b) =>
                write!(f, "Can't switch {a} with {b} after discarding black king"),
            CannotSwitchSamePlayerCards(a, b) =>
                write!(f, "Can't switch {a} and {b} because they belong to the same player"),
            CambioAlreadyCalled(player) =>
                write!(f, "Can't call Cambio because {player} already did so"),
            StickOnEmptyDiscardPile(location) =>
                write!(f, "Can't stick {location} because discard pile is empty"),
            IllegalAction =>
                write!(f, "This action is illegal in the current game state")
        }
    }
}

/// A Cambio game. The only two implemented variants are [DeterminizedGame] and [PartialInfoGame],
/// which are both typealiases. See documentation there for more information.
pub struct Game<UnderlyingCard: UnderlyingCardType> {
    /// The current player to move.
    turn: Player,

    /// The discard pile. Cards are in the order they were discarded.
    discard_pile: Vec<Card>,

    /// All cards that are in an unknown location.
    ///
    /// When [UnderlyingCard] is [Card], this is the draw deck.
    ///
    /// When [UnderlyingCard] is [Option<Card>], this is the draw deck plus all cards that are
    /// floating around in people's piles somewhere.
    unseen_cards: Vec<Card>,

    /// The cards that each player has as well as who has seen them.
    ///
    /// See [CardAndVisibility].
    cards: FastJaggedVec<UnderlyingCard>,

    /// The player who called "Cambio".
    cambio_caller: Option<Player>,

    /// Whether a valid stick attempt has already been made this turn.
    already_stuck: bool,

    /// The number of cards in circulation.
    draw_deck_size: usize,

    /// The current state of the game.
    state: State<UnderlyingCard>
}

impl<UnderlyingCard: UnderlyingCardType + Copy> Game<UnderlyingCard> {
    /// Getter for [turn].
    pub fn turn(&self) -> Player {
        self.turn
    }

    /// Getter for [discard_pile].
    pub fn discard_pile(&self) -> &[Card] {
        &self.discard_pile
    }

    /// Getter for [unseen_cards].
    pub fn unseen_cards(&self) -> &[Card] {
        &self.unseen_cards
    }

    /// Getter for [player_cards].
    pub fn player_cards(&self, player: Player) -> &[CardAndVisibility<UnderlyingCard>] {
        &self.cards[player]
    }

    /// Getter for [cambio_caller].
    pub fn cambio_caller(&self) -> Option<Player> {
        self.cambio_caller
    }

    /// Getter for [already_stuck].
    pub fn already_stuck(&self) -> bool {
        self.already_stuck
    }

    // Getter for [state].
    pub fn state(&self) -> State<UnderlyingCard> {
        self.state
    }

    /// Obtains the range of indices of a player's cards.
    ///
    /// ## Example
    /// If player 2 has 5 cards, then
    /// ```
    /// player_card_indices(2) == 0..5
    /// ```
    fn player_card_indices(&self, player: Player) -> Range<usize> {
        0..self.cards[player].len()
    }

    /// Verifies whether a [CardLocation] points to an existing card in this [Game].
    ///
    /// TODO Verify input with this
    pub fn is_valid_location(&self, location: CardLocation) -> bool {
        (location.player as Player) < self.num_players()
            && self.player_card_indices(location.player as Player).contains(&(location.index as usize))
    }

    /// Increments the turn, rolling back to `0` if necessary.
    fn inc_turn(&mut self) {
        self.turn =
            if self.turn >= self.num_players() - 1 {
                0
            } else {
                self.turn + 1
            };
    }

    /// The player who previously moved.
    pub fn prev_turn(&self) -> Player {
        if self.turn == 0 {
            self.num_players() - 1
        } else {
            self.turn - 1
        }
    }

    /// The number of players in the game.
    pub fn num_players(&self) -> usize {
        self.cards.num_players()
    }

    /// The expected value of an unseen card given the information available in this [Game] state.
    pub fn avg_unseen(&self) -> f32 {
        let sum = self.unseen_cards.iter()
            .map(|card| card.points())
            .sum::<i32>() as f32;
        sum / self.unseen_cards.len() as f32
    }

    /// Returns the next state after discarding a card.
    fn state_after_discarding(card: Card) -> State<UnderlyingCard> {
        match card {
            Card::Seven | Card::Eight =>
            State::AfterDiscard7Or8,
            Card::Nine | Card::Ten =>
            State::AfterDiscard9Or10,
            Card::Jack | Card::Queen | Card::RedKing =>
            State::AfterDiscardFace,
            Card::BlackKing =>
            State::AfterDiscardBlackKing,
            _ => State::EndOfTurn
        }
    }

    /// **Internal use only.** Executes the action and returns an `Ok(Some<State>)` if the action
    /// has the same behavior in [DeterminizedGame] and [PartialInfoGame]. This is to avoid
    /// duplicated code.
    ///
    /// ## Return values
    /// - `Ok(true)` means the action is legal and was executed.
    /// - `Ok(false)` means either (a) the action has behavior specific to [DeterminizedGame] or
    ///   [PartialInfoGame] or (b) the action is not valid in the current [State]. No action is
    ///   taken in either case.
    /// - `Err(_)` means the action is illegal. No action is taken.
    ///
    /// TODO False sticking
    fn execute_if_common_behavior(&mut self, action: Action) -> ExecuteActionResult<bool> {
        match (self.state, action) {
            (_, Action::Stick { stick_player: _, stick_location, give_away_location }) => {
                if !self.is_valid_location(stick_location) {
                    return Err(InvalidCardLocation(stick_location));
                }

                if let Some(give_away_location) = give_away_location
                    && !self.is_valid_location(give_away_location)
                {
                    return Err(InvalidCardLocation(give_away_location));
                }

                /* Execute the stick */
                self.already_stuck = true;
                self.cards.remove_at(stick_location);
                if let Some(&discard_pile_top) = self.discard_pile.last() {
                    self.discard_pile.push(discard_pile_top);
                } else {
                    return Err(StickOnEmptyDiscardPile(stick_location));
                }

                // TODO Update implementation to match rules -- currently ends turn
                // If a card was peeked previously in preparation for a black king swap, we may need to
                // adjust the index of that card so it still points to the same card
                if let State::AfterBlackKingPeeked(peeked_location) = self.state {
                    if peeked_location.player == stick_location.player
                        && peeked_location.index >= stick_location.index
                    {
                        self.state = State::EndOfTurn;
                    } else if let Some(give_away_location) = give_away_location
                        && peeked_location.player == give_away_location.player
                        && peeked_location.index >= stick_location.index
                    {
                        self.state = State::EndOfTurn;
                    }
                }

                /* Execute giveaway */
                if let Some(give_away_location) = give_away_location {
                    self.cards
                        .move_card_to_player(give_away_location, stick_location.player as Player);
                }

                Ok(true)
            }

            (State::BeginningOfTurn, Action::CallCambio) => {
                if let Some(cambio_caller) = self.cambio_caller {
                    return Err(CambioAlreadyCalled(cambio_caller));
                }

                self.cambio_caller = Some(self.turn);
                self.inc_turn();
                self.state = State::BeginningOfTurn;
                Ok(true)
            }

            (State::AfterDiscard7Or8, Action::Peek(location)) => {
                if !self.is_valid_location(location) {
                    return Err(InvalidCardLocation(location));
                }

                if location.player != self.turn as u8 {
                    return Err(CannotPeekOther(location))
                }

                self.cards[location].show_to(self.turn);
                self.state = State::EndOfTurn;
                Ok(true)
            }

            (State::AfterDiscard9Or10, Action::Peek(location)) => {
                if !self.is_valid_location(location) {
                    return Err(InvalidCardLocation(location));
                }

                if location.player == self.turn as u8 {
                    return Err(CannotPeekOwn(location))
                }

                self.cards[location].show_to(self.turn);
                self.state = State::EndOfTurn;
                Ok(true)
            }

            (State::AfterDiscardFace, Action::BlindSwitch(pos_a, pos_b)) => {
                if !self.is_valid_location(pos_a) {
                    return Err(InvalidCardLocation(pos_a));
                }

                if !self.is_valid_location(pos_b) {
                    return Err(InvalidCardLocation(pos_b));
                }

                if pos_a.player == pos_b.player {
                    return Err(CannotSwitchSamePlayerCards(pos_a, pos_b))
                }

                self.cards.swap(pos_a, pos_b);
                self.state = State::EndOfTurn;
                Ok(true)
            }

            (State::AfterBlackKingPeeked(peeked_card),
                Action::BlindSwitch(pos_a, pos_b)
            ) => {
                if !self.is_valid_location(peeked_card) {
                    return Err(InvalidCardLocation(peeked_card));
                }

                if pos_a.player == pos_b.player {
                    return Err(CannotSwitchSamePlayerCards(pos_a, pos_b))
                }

                if pos_a != peeked_card {
                    return Err(InvalidBlackKingSwitch(pos_a, pos_b))
                }

                self.cards.swap(pos_a, pos_b);
                self.state = State::EndOfTurn;
                Ok(true)
            }

            (State::AfterDiscardBlackKing, Action::Peek(location)) => {
                if !self.is_valid_location(location) {
                    return Err(InvalidCardLocation(location));
                }

                if location.player == self.turn as u8 {
                    return Err(CannotPeekOwn(location))
                }

                self.cards[location].show_to(self.turn);
                self.state = State::AfterBlackKingPeeked(location);
                Ok(true)
            }

            (
                State::AfterDiscard7Or8 |
                State::AfterDiscard9Or10 |
                State::AfterDiscardFace |
                State::AfterDiscardBlackKing |
                State::AfterBlackKingPeeked(_),
                Action::SkipOptional
            ) => {
                self.state = State::EndOfTurn;
                Ok(true)
            }

            (State::EndOfTurn, Action::EndTurn) => {
                self.already_stuck = false;
                self.inc_turn();
                self.state = if let Some(cambio_caller) = self.cambio_caller
                    && cambio_caller == self.turn
                {
                    State::EndOfGame
                } else {
                    State::BeginningOfTurn
                };

                Ok(true)
            }

            _ => Ok(false)
        }
    }
}

impl<UnderlyingCard: UnderlyingCardType + Copy> Index<CardLocation> for Game<UnderlyingCard> {
    type Output = CardAndVisibility<UnderlyingCard>;

    fn index(&self, card: CardLocation) -> &Self::Output {
        &self.cards[card]
    }
}

impl<UnderlyingCard: UnderlyingCardType + Copy> IndexMut<CardLocation> for Game<UnderlyingCard> {
    fn index_mut(&mut self, card: CardLocation) -> &mut Self::Output {
        &mut self.cards[card]
    }
}

/// A game where all cards are known and randomly determinized.
///
/// ## Performance
/// This and related methods are meant to facilitate Monte Carlo tree search. Thus, performance is
/// the highest priority. If we run 1 million playouts -- a conservative number -- then each
/// additional microsecond of compute time per playout translates to one additional second to
/// complete the entire search.
pub type DeterminizedGame = Game<Card>;
impl DeterminizedGame {
    /// Returns a [DeterminizedGame] based on a [PartialInfoGame] with unknown cards determinized.
    pub fn randomized_from(partial_info: &PartialInfoGame, rng: &mut CambioRng) -> Self {
        let mut draw_pile = partial_info.unseen_cards
            .clone();

        Self {
            turn: partial_info.turn,

            discard_pile: partial_info.discard_pile.clone(),

            // Determinize unknown cards
            cards: partial_info.cards.determinized(&mut draw_pile, rng),

            cambio_caller: partial_info.cambio_caller,

            already_stuck: partial_info.already_stuck,

            draw_deck_size: partial_info.draw_deck_size,

            state: match partial_info.state {
                State::AfterDrawing(None) =>
                    State::AfterDrawing(
                        remove_random_from(&mut draw_pile, rng)
                    ),
                // Need to cover all other cases manually because Rust doesn't recognize that all these other cases are the same
                State::AfterDrawing(Some(card)) => State::AfterDrawing(card),
                State::BeginningOfTurn => State::BeginningOfTurn,
                State::AfterDiscard7Or8 => State::AfterDiscard7Or8,
                State::AfterDiscard9Or10 => State::AfterDiscard9Or10,
                State::AfterDiscardFace => State::AfterDiscardFace,
                State::AfterDiscardBlackKing => State::AfterDiscardBlackKing,
                State::AfterBlackKingPeeked(location) => State::AfterBlackKingPeeked(location),
                State::EndOfTurn => State::EndOfTurn,
                State::EndOfGame => State::EndOfGame,
            },

            // This is last because we want the draw pile after determinized cards have been taken out
            unseen_cards: draw_pile
        }
    }

    /// Returns the number of points each player currently has.
    pub fn scores(&self) -> Vec<i32> {
        self.cards.scores()
    }

    /// Returns the list of players who won or tied for winning the game.
    pub fn winners(&self) -> Vec<Player> {
        let scores = self.scores();
        let winning_score = *scores.iter().min()
            .expect("There should be at least one player to choose a winning score from");

        let tied_winners: Vec<Player> =
            scores.into_iter().enumerate()
                .filter_map(|(player, score)|
                    if score == winning_score {
                        Some(player)
                    } else {
                        None
                    }
                )
                .collect();

        if tied_winners.len() <= 1 {
            tied_winners
        } else {
            tied_winners.into_iter()
                .filter(|&player| Some(player) != self.cambio_caller)
                .collect()
        }
    }

    /// Removes and returns a random card from the draw pile.
    ///
    /// If the draw pile is empty after this operation, the discard pile is automatically emptied
    /// into the draw pile. The draw pile is not shuffled.
    ///
    /// This does not change [state] because drawing a card can happen as a result of a false stick,
    /// so it would be wrong to assume that the state should necessarily progress to [AfterDrawing].
    fn draw_from_unseen(&mut self, rng: &mut CambioRng) -> Card {
        let drawn_card = remove_random_from(&mut self.unseen_cards, rng);

        if self.draw_deck_size == 0 {
            self.unseen_cards.append(&mut self.discard_pile);

            // Still empty, meaning discard pile had nothing and the game is effectively over
            // because no draws can happen
            // TODO Loophole here
        }

        drawn_card
    }

    /// Extends with the sticks that each player would know to be correct. That is, not only does
    /// this filter for correct sticks, it also filters for correct sticks where the player sticking
    /// the card has seen that card and thus knows it will be successful.
    ///
    /// ## Why not just return all possible sticks?
    ///
    /// These two filters save time on two accounts:
    /// - We don't waste time considering false sticks, which are legal but unlikely to happen on
    ///   purpose. If it's on accident, it essentially always improves our chances of winning, so we
    ///   are better off assuming people won't sabotage themselves by making false sticks
    ///   deliberately.
    /// - We don't waste time considering any sticks, even valid sticks, on cards that the player
    ///   sticking has not seen. This results from the assumption that players will only stick cards
    ///   that they know to be valid sticks, so if they haven't seen a card they won't try to stick
    ///   it.
    fn extend_with_realistic_sticks(&self, actions: &mut Vec<Action>) {
        if self.already_stuck || self.discard_pile.is_empty() {
            return;
        }

        // Any card equal to this can be stuck successfully
        let card_to_match = *self.discard_pile.last()
            .expect("Discard pile shouldn't be empty");

        actions.extend(self.cards.enumerate()
            .into_iter()
            // Filter to valid sticks only
            .filter_map(|(stick_location, card)|
                if card.value == card_to_match
                    // Cambio caller cannot be affected by sticks
                    && self.cambio_caller != Some(stick_location.player as Player)
                {
                    Some(
                        // Find all players who can see this card and let them stick it
                        (0..self.num_players())
                            .filter(|stick_player| card.seen_by(*stick_player))
                            .flat_map(move |stick_player| {
                                // You don't have to give away one of your cards even if it's
                                // another player's card that you stuck
                                let mut actions = vec![Action::Stick {
                                    stick_player,
                                    stick_location, give_away_location: None
                                }];
                                // Stick another player's card
                                if stick_player != stick_location.player as usize {
                                    actions.extend(self.player_card_indices(stick_player)
                                        .map(|index|
                                            Action::Stick {
                                                stick_player,
                                                stick_location,
                                                give_away_location: Some(CardLocation::new(stick_player, index)),
                                            }
                                        ));
                                }
                                actions
                            })
                    )
                } else {
                    None
                }
            )
            .flatten()
        );
    }

    /// Returns all legal moves from this game.
    ///
    /// This excludes what I call "unrealistic sticks", i.e. sticks that someone might reasonably
    /// play. The rule is that, almost always, there is no reason to stick a card you do not know
    /// or attempt a stick that you know is invalid.
    pub fn legal_actions(&self) -> Vec<Action> {
        match self.state {
            State::BeginningOfTurn =>
                if self.cambio_caller.is_some() {
                    vec![Action::Draw]
                } else {
                    vec![Action::Draw, Action::CallCambio]
                }

            State::AfterDrawing(_) => {
                let mut actions = Vec::from_iter(
                    // Find indices of own cards
                    self.player_card_indices(self.turn)
                        .map(|index|
                            Action::SwapDrawnCardForOwn(CardLocation::new(self.turn, index))
                        )
                );
                actions.push(Action::Discard);
                actions
            }

            State::AfterDiscard7Or8 => {
                let mut actions = Vec::from_iter(
                    // Find indices of own cards
                    self.player_card_indices(self.turn)
                        .map(|index|
                            Action::Peek(CardLocation::new(self.turn, index))
                        )
                );

                self.extend_with_realistic_sticks(&mut actions);
                actions.push(Action::SkipOptional);
                actions
            }

            State::AfterDiscard9Or10 | State::AfterDiscardBlackKing => {
                let mut actions = Vec::from_iter(
                    self.cards.locations_from_player(0)
                        .into_iter()
                        // Remove own cards
                        .filter_map(|location|
                            if location.player == self.turn as u8 {
                                None
                            } else {
                                Some(Action::Peek(location))
                            }
                        )
                );

                self.extend_with_realistic_sticks(&mut actions);
                actions.push(Action::SkipOptional);
                actions
            }

            State::AfterDiscardFace => {
                let mut actions: Vec<Action> =
                    self.cards.locations_from_player(0)
                        .into_iter()
                        .flat_map(|location_a|
                            self.cards.locations_from_player(location_a.player as Player + 1)
                                .into_iter()
                                .filter_map(|location_b|
                                    // Cambio caller can't be affected by swaps
                                    if Some(location_a.player as Player) == self.cambio_caller
                                        || Some(location_b.player as Player) == self.cambio_caller
                                    {
                                        None
                                    } else {
                                        Some(Action::BlindSwitch(location_a, location_b))
                                    }
                                )
                                .collect::<Vec<_>>()
                        )
                        .collect();

                self.extend_with_realistic_sticks(&mut actions);
                actions.push(Action::SkipOptional);
                actions
            }

            State::AfterBlackKingPeeked(location) => {
                let mut actions =
                    // Cambio caller cannot be affected by swaps
                    if Some(location.player as Player) == self.cambio_caller {
                        vec![]
                    } else {
                        Vec::from_iter(
                            self.player_card_indices(self.turn)
                                .map(|index|
                                    Action::BlindSwitch(location, CardLocation::new(self.turn, index))
                                )
                        )
                    };

                self.extend_with_realistic_sticks(&mut actions);
                actions.push(Action::SkipOptional);
                actions
            }

            State::EndOfTurn => {
                let mut actions = vec![Action::EndTurn];
                self.extend_with_realistic_sticks(&mut actions);
                actions
            }

            State::EndOfGame => vec![]
        }
    }

    /// Executes an action.
    pub fn execute(&mut self, action: Action, rng: &mut CambioRng) -> ExecuteActionResult<()> {
        // If this action is implemented for both [DeterminizedGame] and [PartialInfoGame], use that
        // code
        match self.execute_if_common_behavior(action) {
            Ok(true) => {
                return Ok(())
            }

            // See if there's any specific behavior below
            Ok(false) => {}

            Err(reason) => {
                return Err(reason)
            }
        }

        match (&self.state, action) {
            (State::BeginningOfTurn, Action::Draw) => {
                self.draw_deck_size -= 1;
                self.state = State::AfterDrawing(self.draw_from_unseen(rng));
                Ok(())
            }

            (State::AfterDrawing(card), Action::Discard) => {
                self.discard_pile.push(*card);
                self.state = Self::state_after_discarding(*card);
                Ok(())
            }

            (State::AfterDrawing(drawn_card), Action::SwapDrawnCardForOwn(location)) => {
                if !self.is_valid_location(location) {
                    return Err(InvalidCardLocation(location));
                }

                if location.player != self.turn as u8 {
                    return Err(CannotSwapWithOther(location));
                }

                // Add the original card at [location] to the discard pile
                self.discard_pile.push(
                    self.cards[location].value
                );
                // Replace the card at [location] and indicate it's been seen by this player
                self.cards[location] =
                    CardAndVisibility::new_seen_by_one(*drawn_card, self.turn);

                self.state = State::EndOfTurn;
                Ok(())
            }

            // Illegal action
            _ => Err(IllegalAction)
        }
    }
}

/// A game with all the information known to player 0.
pub type PartialInfoGame = Game<Option<Card>>;
impl PartialInfoGame {
    /// Initializes a new [PartialInfoGame].
    ///
    /// [bottom_left] and [bottom_right] are the cards that player 0 gets to see at the beginning of
    /// the game. They are stored in indices 2 and 3 respectively.
    pub fn new(num_players: usize, first_player: Player, jokers: bool, bottom_left: Card, bottom_right: Card) -> Self {
        let deck: Vec<_> = [
            // Map-like array of card frequencies
            (Card::Ace, 4),
            (Card::Two, 4),
            (Card::Three, 4),
            (Card::Four, 4),
            (Card::Five, 4),
            (Card::Six, 4),
            (Card::Seven, 4),
            (Card::Eight, 4),
            (Card::Nine, 4),
            (Card::Ten, 4),
            (Card::Jack, 4),
            (Card::Queen, 4),
            (Card::BlackKing, 2),
            (Card::RedKing, 2),
            (Card::Joker, if jokers { 2 } else { 0 }),
        ].into_iter()
            // Map to collection of &Card
            .flat_map(|(card, freq)|
                iter::repeat_n(card, freq)
            )
            // Collect into Vec<Card>
            .collect();
        let draw_deck_size = deck.len();

        let mut result = Self {
            turn: first_player,
            discard_pile: Vec::new(),
            unseen_cards: deck,
            cards: FastJaggedVec::new(num_players),
            already_stuck: false,
            cambio_caller: None,
            draw_deck_size,
            state: State::BeginningOfTurn
        };

        result[CardLocation::new(0, 2)] = CardAndVisibility::new_seen_by_one(Some(bottom_left), 0);
        result[CardLocation::new(0, 3)] = CardAndVisibility::new_seen_by_one(Some(bottom_right), 0);

        result
    }

    /// Removes a card from [unseen_cards]. This is a formality asking the [PartialInfoGame] whether
    /// it is possible that [revealed_card] can be removed from the draw deck in this state -- if so,
    /// it removes it from [unseen_cards].
    ///
    /// This does not change [state] because drawing a card can happen as a result of a false stick,
    /// so it would be wrong to assume that the state should necessarily progress to [AfterDrawing].
    fn remove_from_unseen(&mut self, revealed_card: Card) -> Result<Card, ()> {
        if !self.unseen_cards.contains(&revealed_card) {
            // Drawing this card is impossible because we know it cannot be in the draw pile
            return Err(())
        }

        // Remove the first instance of [card_drawn]
        self.unseen_cards.remove(
            self.unseen_cards.iter()
                .position(|&c| c == revealed_card)
                .unwrap()
        );

        // Find the number of unseen cards among players' piles. If the total number of unseen cards
        // is equal to this then it means there are no cards in the draw pile and we need to reshuffle.
        let num_unseen_player_cards = self.cards
            .flatten()
            .iter()
            .filter(|exposed_card| exposed_card.value.is_none())
            .count();

        if self.unseen_cards.len() <= num_unseen_player_cards {
            self.unseen_cards.append(&mut self.discard_pile);
        }

        Ok(revealed_card)
    }

    /// If [action] is legal, it is executed, and [Ok] is returned; otherwise [Err] is returned and
    /// the game state is not mutated.
    pub fn execute(&mut self, action: Action, revealed_card: Option<Card>) -> ExecuteActionResult<()> {
        // If this action is implemented for both [DeterminizedGame] and [PartialInfoGame], use that
        // code
        match self.execute_if_common_behavior(action) {
            Ok(true) => {
                if let Action::Peek(location) = action
                    && revealed_card.is_some()
                {
                    self.cards[location].value = revealed_card;
                }
                return Ok(());
            }

            Ok(false) => {}

            Err(reason) => {
                return Err(reason)
            }
        }

        // Revealed a card that cannot be in [unseen_cards]?
        if let Some(revealed_card) = revealed_card
            && !self.unseen_cards.contains(&revealed_card)
        {
            return Err(ImpossibleUnseenCard(revealed_card));
        }

        match (self.state, action) {
            (State::BeginningOfTurn, Action::Draw) => {
                self.draw_deck_size -= 1;
                self.state = State::AfterDrawing(revealed_card);
                if let Some(revealed_card) = revealed_card {
                    self.remove_from_unseen(revealed_card)
                        .expect("Revealed card isn't in unseen cards");
                }
                Ok(())
            }

            (State::AfterDrawing(drawn_card), Action::Discard) => {
                match Card::pick_known(drawn_card, revealed_card) {
                    PickKnownCardResult::Ok(known_drawn_card) => {
                        self.discard_pile.push(known_drawn_card);
                        self.state = Self::state_after_discarding(known_drawn_card);
                        if let Some(revealed_card) = revealed_card {
                            self.remove_from_unseen(revealed_card)
                                .expect("Revealed card isn't in unseen cards");
                        }
                        Ok(())
                    }
                    PickKnownCardResult::Conflicting(actual_card, attested_card) => {
                        Err(ConflictingInfo { actual_card, attested_card })
                    }
                    PickKnownCardResult::Unknown => {
                        Err(NotEnoughInfo)
                    }
                }
            }

            (State::AfterDrawing(drawn_card), Action::SwapDrawnCardForOwn(location)) => {
                if !self.is_valid_location(location) {
                    return Err(InvalidCardLocation(location));
                }

                if location.player != self.turn as u8 {
                    return Err(CannotSwapWithOther(location));
                }

                // Try to find the original card at [location] or get it from [revealed_card]
                match Card::pick_known(self.cards[location].value, revealed_card) {
                    PickKnownCardResult::Ok(known_replaced_card) => {
                        // Add the original card at [location] to the discard pile
                        self.discard_pile.push(known_replaced_card);

                        // Replace the card at [location] and indicate it's been seen by this player
                        self.cards[location] =
                            CardAndVisibility::new_seen_by_one(drawn_card, self.turn);

                        self.state = State::EndOfTurn;

                        if let Some(revealed_card) = revealed_card {
                            self.remove_from_unseen(revealed_card)
                                .expect("Revealed card isn't in unseen cards");
                        }

                        Ok(())
                    }
                    PickKnownCardResult::Conflicting(actual_card, attested_card) =>
                        Err(ConflictingInfo { actual_card, attested_card }),
                    PickKnownCardResult::Unknown =>
                        Err(NotEnoughInfo)
                }
            }

            // Illegal action
            _ => Err(IllegalAction)
        }
    }

    /// Calculates the size of the draw pile.
    pub fn draw_deck_size(&self) -> usize {
        self.draw_deck_size
    }
}