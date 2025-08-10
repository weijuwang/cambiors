use super::*;
use fast_jagged_vec::*;

use std::iter;
use std::ops::*;
use std::cmp::Ordering;

/// A Cambio game. The only two implemented variants are [DeterminizedGame] and [PartialInfoGame],
/// which are both typealiases. See documentation there for more information.
pub struct Game<UnderlyingCard: UnderlyingCardType> {
    /// The current player to move.
    turn: Player,

    /// The discard pile. Cards are in the order they were discarded.
    discard_pile: Vec<Card>,

    /// All cards that are in an unknown position.
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

    /// **Internal use only.** Executes the action and returns a `Some<State>` if the action
    /// has the same behavior in [DeterminizedGame] and [PartialInfoGame]. This is to avoid
    /// duplicated code.
    fn execute_if_common_behavior(&mut self, action: Action) -> Option<State<UnderlyingCard>> {
        match (self.state, action) {
            (_, Action::StickWithoutGiveAway(stick_position)) => {
                // Execute the stick
                self.already_stuck = true;
                self.cards.remove_at(stick_position);
                self.discard_pile.push(
                    *self.discard_pile.last()
                        .expect("Cannot stick without something on the discard pile")
                );

                // If a card was peeked previously in preparation for a black king swap, we may need to
                // adjust the index of that card so it still points to the same card
                /*
                The purpose of this code is to prevent the following scenario:

                Alice discards a black king and peeks at one of Bob's cards. Before Alice can decide
                what to do with it, one of the following happens:
                A: Any player sticks one of Bob's other cards. Since Bob now has one less card, any
                  [CardPosition] pointing to one of Bob's cards might not point to the same card it
                  originally did -- elements shift when one is removed. Therefore, we need to update the
                  state so the position points to the same card.
                B: Any player sticks the card that Alice peeked. (This scenario applies to the code for
                  [StickWithoutGiveAway] above as well.) Alice needs to choose a new card.
                C: Bob sticks someone else's card and gives away the card Alice peeked. The position of
                  the card Alice peeked at needs to be updated to point to the same card now belonging
                  to a different player.

                TODO Is there seriously not a better way to do this? Maybe immut reference to card instead
                of card positions? I'm basically managing pointers manually which is not very safe
                 */
                Some(if let State::AfterBlackKingPeeked(peeked_position) = self.state
                    // If the peeked card and the stuck card belong to the same player
                    && peeked_position.player == stick_position.player {
                    match peeked_position.index.cmp(&stick_position.index) {
                        Ordering::Less =>
                            self.state,
                        // ~~~ SCENARIO B ~~~
                        // `stick_position == peeked_position`
                        // The card that got peeked at got stuck
                        // Let the player peek another one
                        Ordering::Equal =>
                            State::AfterDiscardBlackKing,
                        // ~~~ SCENARIO A ~~~
                        // [peeked_position] points to something different from what it did originally
                        // because everything got shifted over
                        Ordering::Greater =>
                            State::AfterBlackKingPeeked(CardPosition {
                                player: peeked_position.player, index: peeked_position.index - 1
                            })
                    }
                } else { self.state })
            }

            (_, Action::StickWithGiveAway { stick_position, give_away_position }) => {
                // Execute the stick
                self.already_stuck = true;
                self.cards.remove_at(stick_position);
                self.discard_pile.push(
                    *self.discard_pile.last()
                        .expect("Cannot stick without something on the discard pile")
                );

                // If a card was peeked previously in preparation for a black king swap, we may need to
                // adjust the index of that card so it still points to the same card
                let new_state = if let State::AfterBlackKingPeeked(peeked_position) = self.state {
                    if peeked_position.player == stick_position.player {
                        // If the peeked card and the stuck card belong to the same player
                        match peeked_position.index.cmp(&stick_position.index) {
                            Ordering::Less =>
                                self.state,
                            // ~~~ SCENARIO B ~~~
                            // `stick_position == peeked_position`
                            // The card that got peeked at got stuck
                            // Let the player peek another one
                            Ordering::Equal =>
                                State::AfterDiscardBlackKing,
                            // ~~~ SCENARIO A ~~~
                            // [peeked_position] points to something different from what it did originally
                            // because everything got shifted over
                            Ordering::Greater =>
                                State::AfterBlackKingPeeked(CardPosition {
                                    player: peeked_position.player,
                                    index: peeked_position.index - 1
                                })
                        }
                    } else if peeked_position.player == give_away_position.player {
                        // If the peeked card and the give-away card belong to the same player
                        match peeked_position.index.cmp(&stick_position.index) {
                            Ordering::Less =>
                                self.state,
                            // ~~~ SCENARIO C ~~~
                            // `stick_position == give_away_position`
                            // The peeked card was given away
                            Ordering::Equal =>
                                State::AfterBlackKingPeeked(CardPosition {
                                    // The peeked card has been given away to the player whose
                                    // card was stuck
                                    player: stick_position.player,
                                    // This is the CURRENT number of cards they have; they will have
                                    // one more than this after the give-away
                                    index: self.cards[stick_position.player as Player]
                                        .len() as u8
                                }),
                            // ~~~ SCENARIO A ~~
                            // [peeked_position] points to something different from what it
                            // did originally because everything got shifted over
                            Ordering::Greater =>
                                State::AfterBlackKingPeeked(CardPosition {
                                    player: peeked_position.player,
                                    index: peeked_position.index - 1
                                })
                        }
                    } else { self.state }
                } else { self.state };

                // Execute giveaway
                self.cards
                    .move_card_to_player(give_away_position, stick_position.player as Player);

                Some(new_state)
            }

            (State::BeginningOfTurn, Action::CallCambio) => {
                self.cambio_caller = Some(self.turn);
                self.inc_turn();
                Some(State::BeginningOfTurn)
            }

            (State::AfterDiscard7Or8, Action::Peek(position))
            if position.player == self.turn as u8 => {
                self.cards[position].show_to(self.turn);
                Some(State::EndOfTurn)
            }

            (State::AfterDiscard9Or10, Action::Peek(position))
            if position.player != self.turn as u8 => {
                self.cards[position].show_to(self.turn);
                Some(State::EndOfTurn)
            }

            (State::AfterDiscardFace, Action::BlindSwitch(pos_a, pos_b))
            if pos_a.player != pos_b.player => {
                self.cards.swap(pos_a, pos_b);
                Some(State::EndOfTurn)
            }

            (State::AfterBlackKingPeeked(peeked_card), Action::BlindSwitch(pos_a, pos_b))
            if pos_a == peeked_card => {
                self.cards.swap(pos_a, pos_b);
                Some(State::EndOfTurn)
            }

            (State::AfterDiscardBlackKing, Action::Peek(position))
            if position.player != self.turn as u8 => {
                self.cards[position].show_to(self.turn);
                Some(State::AfterBlackKingPeeked(position))
            }

            (
                State::AfterDiscard7Or8 |
                State::AfterDiscard9Or10 |
                State::AfterDiscardFace |
                State::AfterDiscardBlackKing |
                State::AfterBlackKingPeeked(_),
                Action::SkipOptional
            ) => Some(State::EndOfTurn),

            (State::EndOfTurn, Action::EndTurn) => {
                self.already_stuck = false;
                self.inc_turn();

                Some(
                    if let Some(cambio_caller) = self.cambio_caller
                    && cambio_caller == self.turn
                    {
                        State::EndOfGame
                    } else {
                        State::BeginningOfTurn
                    }
                )
            }

            _ => None
        }
    }
}

impl<UnderlyingCard: UnderlyingCardType + Copy> Index<CardPosition> for Game<UnderlyingCard> {
    type Output = CardAndVisibility<UnderlyingCard>;

    fn index(&self, card: CardPosition) -> &Self::Output {
        &self.cards[card]
    }
}

impl<UnderlyingCard: UnderlyingCardType + Copy> IndexMut<CardPosition> for Game<UnderlyingCard> {
    fn index_mut(&mut self, card: CardPosition) -> &mut Self::Output {
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
                State::AfterBlackKingPeeked(position) => State::AfterBlackKingPeeked(position),
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
    fn draw_random_card(&mut self, rng: &mut CambioRng) -> Card {
        let drawn_card = remove_random_from(&mut self.unseen_cards, rng);

        if self.unseen_cards.is_empty() {
            self.unseen_cards.append(&mut self.discard_pile);

            // Still empty, meaning discard pile had nothing and the game is effectively over
            // because no draws can happen

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
        if self.already_stuck {
            return;
        }

        // Any card equal to this can be stuck successfully
        let card_to_match = *self.discard_pile.last()
            .expect("Discard pile shouldn't be empty");

        actions.extend(self.cards.enumerate()
            .into_iter()
            // Filter to valid sticks only
            .filter_map(|(position, card)|
                if card.value() == card_to_match
                    // Cambio caller cannot be affected by sticks
                    && self.cambio_caller != Some(position.player as Player)
                {
                    Some(
                        // Find all players who can see this card and let them stick it
                        (0..self.num_players())
                            .filter(|player| card.seen_by(*player))
                            .flat_map(move |player| {
                                // You don't have to give away one of your cards even if it's
                                // another player's card that you stuck
                                let mut actions = vec![Action::StickWithoutGiveAway(position)];
                                // Stick another player's card
                                if player != position.player as usize {
                                    actions.extend(self.player_card_indices(player)
                                        .map(|index|
                                            Action::StickWithGiveAway {
                                                stick_position: position,
                                                give_away_position: CardPosition {
                                                    player: player as u8,
                                                    index: index as u8
                                                }
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

    /// Returns all legal moves from this position.
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
                            Action::Swap(CardPosition { player: self.turn as u8, index: index as u8 })
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
                            Action::Peek(CardPosition { player: self.turn as u8, index: index as u8 })
                        )
                );

                self.extend_with_realistic_sticks(&mut actions);
                actions.push(Action::SkipOptional);
                actions
            }

            State::AfterDiscard9Or10 | State::AfterDiscardBlackKing => {
                let mut actions = Vec::from_iter(
                    self.cards.positions_from_player(0)
                        .into_iter()
                        // Remove own cards
                        .filter_map(|position|
                            if position.player == self.turn as u8 {
                                None
                            } else {
                                Some(Action::Peek(position))
                            }
                        )
                );

                self.extend_with_realistic_sticks(&mut actions);
                actions.push(Action::SkipOptional);
                actions
            }

            State::AfterDiscardFace => {
                let mut actions: Vec<Action> =
                    self.cards.positions_from_player(0)
                        .into_iter()
                        .flat_map(|position_a|
                            self.cards.positions_from_player(position_a.player as Player + 1)
                                .into_iter()
                                .filter_map(|position_b|
                                    // Cambio caller can't be affected by swaps
                                    if Some(position_a.player as Player) == self.cambio_caller
                                        || Some(position_b.player as Player) == self.cambio_caller
                                    {
                                        None
                                    } else {
                                        Some(Action::BlindSwitch(position_a, position_b))
                                    }
                                )
                                .collect::<Vec<_>>()
                        )
                        .collect();

                self.extend_with_realistic_sticks(&mut actions);
                actions.push(Action::SkipOptional);
                actions
            }

            State::AfterBlackKingPeeked(position) => {
                let mut actions =
                    // Cambio caller cannot be affected by swaps
                    if Some(position.player as Player) == self.cambio_caller {
                        vec![]
                    } else {
                        Vec::from_iter(
                            self.player_card_indices(self.turn)
                                .map(|index|
                                    Action::BlindSwitch(position, CardPosition { player: self.turn as u8, index: index as u8 })
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
    ///
    /// **This does not check whether the action is legal** to save computation time. Use
    /// [legal_actions] to check manually.
    ///
    /// Any stick will go through regardless of whether the card is correct. All other actions will
    /// work whenever the state is correct, but no other conditions are checked.
    pub fn execute(&mut self, action: Action, rng: &mut CambioRng) {
        // If this action is implemented for both [DeterminizedGame] and [PartialInfoGame], use that
        // code
        if let Some(new_state) = self.execute_if_common_behavior(action) {
            self.state = new_state;
            return;
        }

        self.state = match (&self.state, action) {
            (State::BeginningOfTurn, Action::Draw) =>
                State::AfterDrawing(self.draw_random_card(rng)),

            (State::AfterDrawing(card), Action::Discard) => {
                self.discard_pile.push(*card);
                Self::state_after_discarding(*card)
            }

            (State::AfterDrawing(drawn_card), Action::Swap(position)
            ) if !self.cards[self.turn].is_empty() => {
                // Add the original card at [position] to the discard pile
                self.discard_pile.push(
                    self.cards[position].value()
                );
                // Replace the card at [position] and indicate it's been seen by this player
                self.cards[position] =
                    CardAndVisibility::new_seen_by_one(*drawn_card, self.turn);

                State::EndOfTurn
            }

            // Illegal action
            _ => panic!("Illegal action attempted on DeterminizedGame")
        };
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
        let mut result = Self {
            turn: first_player,
            discard_pile: Vec::new(),
            unseen_cards: [
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
                .collect::<Vec<Card>>(),

            cards: FastJaggedVec::new(num_players, bottom_left, bottom_right),
            already_stuck: false,
            cambio_caller: None,
            state: State::BeginningOfTurn
        };

        result[CardPosition { player: 0, index: 2 }] = CardAndVisibility::new_seen_by_one(Some(bottom_left), 0);
        result[CardPosition { player: 0, index: 3 }] = CardAndVisibility::new_seen_by_one(Some(bottom_right), 0);

        result
    }

    /// Removes a card from the draw pile. This is a formality asking the [PartialInfoGame] whether
    /// it is possible that [card_drawn] can be removed from the draw deck in this state -- if so,
    /// it removes it from [unseen_cards].
    ///
    /// This does not change [state] because drawing a card can happen as a result of a false stick,
    /// so it would be wrong to assume that the state should necessarily progress to [AfterDrawing].
    fn pop_draw_pile(&mut self, card_drawn: Card) -> Option<Card> {
        if !self.unseen_cards.contains(&card_drawn) {
            // Drawing this card is impossible because we know it cannot be in the draw pile
            return None
        }

        // Remove the first instance of [card_drawn]
        self.unseen_cards.remove(
            self.unseen_cards.iter()
                .position(|&c| c == card_drawn)
                .unwrap()
        );

        // TODO This is an expensive way of finding whether the draw pile is empty. However, it is easier to code given Rust's restrictions on OOP, and we don't care about speed because it's not in [DeterminizedGame]. I would like to find a more elegant way to do this in the future.
        // Find the number of unseen cards among players' piles. If the total number of unseen cards
        // is equal to this then it means there are no cards in the draw pile and we need to reshuffle.
        let num_unseen_player_cards = self.cards
            .flatten()
            .iter()
            .filter(|exposed_card| exposed_card.value().is_none())
            .count();

        if self.unseen_cards.len() <= num_unseen_player_cards {
            self.unseen_cards.append(&mut self.discard_pile);
        }

        Some(card_drawn)
    }

    /// If [action] is legal, it is executed, and [Ok] is returned; otherwise [Err] is returned and
    /// the game state is not mutated.
    fn execute(&mut self, action: Action, revealed_card: Option<Card>) -> Result<(), ()> {
        // If this action is implemented for both [DeterminizedGame] and [PartialInfoGame], use that
        // code
        if let Some(new_state) = self.execute_if_common_behavior(action) {
            self.state = new_state;
            return Ok(());
        }

        self.state = match (&self.state, action) {
            (State::BeginningOfTurn, Action::Draw) =>
                State::AfterDrawing(revealed_card),

            (State::AfterDrawing(drawn_card), Action::Discard) => {
                match Card::pick_known(*drawn_card, revealed_card) {
                    Ok(known_drawn_card) => {
                        self.discard_pile.push(known_drawn_card);
                        Self::state_after_discarding(known_drawn_card)
                    }
                    Err(()) => return Err(())
                }
            }

            (State::AfterDrawing(drawn_card), Action::Swap(position)
            ) if !self.cards[self.turn].is_empty() => {
                // Try to find the original card at [position] or get it from [revealed_card]
                match Card::pick_known(self.cards[position].value(), revealed_card) {
                    Ok(known_replaced_card) => {
                        // Add the original card at [position] to the discard pile
                        self.discard_pile.push(known_replaced_card);

                        // Replace the card at [position] and indicate it's been seen by this player
                        self.cards[position] =
                            CardAndVisibility::new_seen_by_one(*drawn_card, self.turn);
                    }
                    Err(()) => return Err(())
                }

                State::EndOfTurn
            }

            // Illegal action
            _ => return Err(())
        };

        Ok(())
    }
}