use super::*;
use std::ops::*;

/// A fast jagged two-dimensional vector for storing [CardAndVisibility]s.
///
/// Internally, the vector is stored flat, and another vector is used for keeping track of indices.
///
/// Cloning this will produce a deep copy.
#[derive(Clone)]
pub(super) struct FastJaggedVec<T: UnderlyingCardType> {
    /// All cards stored in order from player 0's 0th card to the last player's last card.
    cards: Vec<CardAndVisibility<T>>,

    /// The index of each player's first card, plus an element at the end which should always be the
    /// length of [cards].
    player_start_indices: Vec<usize>
}

impl<T: UnderlyingCardType + Copy> FastJaggedVec<T> {
    /// The number of players in the game.
    pub fn num_players(&self) -> usize {
        self.player_start_indices.len() - 1
    }

    /// Returns all cards flattened, as flattening a 2D vector would do.
    pub fn flatten(&self) -> &Vec<CardAndVisibility<T>> {
        &self.cards
    }

    /// Returns all the cards flattened with their indices.
    pub fn enumerate(&self) -> Vec<(CardPosition, &CardAndVisibility<T>)> {
        self.cards.iter().enumerate()
            .map(|(raw_index, card)| {
                let player = (0..self.num_players())
                    .filter(|p| raw_index >= self.player_start_indices[*p])
                    .max()
                    .expect("Something's wrong");
                (CardPosition {
                    player: player as u8,
                    index: (raw_index - self.player_start_indices[player]) as u8
                }, card)
            })
            .collect()
    }

    /// Returns all card positions for players who indices are greater than or equal to
    /// [starting_from].
    pub fn positions_from_player(&self, starting_from: usize) -> Vec<CardPosition> {
        (self.player_start_indices[starting_from]..self.cards.len())
            .map(|raw_index| {
                let player = (starting_from..self.num_players())
                    .filter(|p| raw_index >= self.player_start_indices[*p])
                    .max()
                    .expect("Something's wrong");
                CardPosition {
                    player: player as u8,
                    index: (raw_index - self.player_start_indices[player]) as u8
                }
            })
            .collect()
    }

    /// Returns the number of cards a player has.
    pub fn player_num_cards(&self, player: Player) -> usize {
        self.player_start_indices[player + 1] - self.player_start_indices[player]
    }

    /// Swaps two cards.
    pub fn swap(&mut self, a: CardPosition, b: CardPosition) {
        let idx_a = self.raw_index(a);
        let idx_b = self.raw_index(b);
        self.cards.swap(idx_a, idx_b);
    }

    /// Remove the card at [position] and return the card that was there. All cards with higher
    /// indices or higher player numbers are shifted over.
    pub fn remove_at(&mut self, position: CardPosition) -> T {
        let removed_card = self.cards.remove(self.raw_index(position));

        // Shift player indices over
        for player in position.player as usize + 1 ..= self.num_players() {
            self.player_start_indices[player] -= 1;
        }

        removed_card.value()
    }

    /// Give a player one card.
    pub fn add_to_player(&mut self, player: Player, card: CardAndVisibility<T>) {
        self.cards.insert(self.player_start_indices[player + 1], card);

        // Shift player indices over
        for i in (player as usize + 1) ..= self.num_players() {
            self.player_start_indices[i] += 1;
        }
    }

    /// Moves a card to the end of another player's pile.
    pub fn move_card_to_player(&mut self, original_position: CardPosition, player: Player) {
        let moved_card = self[original_position];
        self.remove_at(original_position);
        self.add_to_player(player, moved_card);
        
        // TODO Better implementation that doesn't go back and forth with index shifting
    }

    /// Obtains the raw index in [player_indices] that corresponds to the card referred to by
    /// [position].
    fn raw_index(&self, position: CardPosition) -> usize {
        self.player_start_indices[position.player as usize]
            + position.index as usize
    }
}

impl FastJaggedVec<Card> {
    /// Returns the number of points each player currently has.
    pub fn scores(&self) -> Vec<i32> {
        (0..self.num_players())
            .map(|player| {
                let start_index = self.player_start_indices[player];
                let end_index = self.player_start_indices[player + 1];
                self.cards[start_index..end_index].iter()
                    .map(|card| card.value().points())
                    .sum()
            })
            .collect()
    }
}

impl FastJaggedVec<Option<Card>> {
    /// Constructs a new [FastJaggedVec] representing what player 0 knows about the cards at the
    /// beginning of the game. [bottom_left] and [bottom_right] are the cards that player 0 gets to
    /// see at the beginning of the game.
    pub fn new(num_players: usize, bottom_left: Card, bottom_right: Card) -> Self {
        Self {
            cards: (0..num_players)
                .flat_map(|player|
                    if player == 0 {
                        vec![
                            CardAndVisibility::new_seen_by_nobody(None),
                            CardAndVisibility::new_seen_by_nobody(None),
                            CardAndVisibility::new_seen_by_one(Some(bottom_left), player),
                            CardAndVisibility::new_seen_by_one(Some(bottom_right), player),
                        ]
                    } else {
                        vec![
                            CardAndVisibility::new_seen_by_nobody(None),
                            CardAndVisibility::new_seen_by_nobody(None),
                            CardAndVisibility::new_seen_by_one(None, player),
                            CardAndVisibility::new_seen_by_one(None, player),
                        ]
                    }
                )
                .collect(),
            player_start_indices: (0..=num_players)
                .map(|p| p * 4)
                .collect()
        }
    }

    /// Returns a randomly determinized version of each players' cards.
    ///
    /// When a card needs to be determinized, it is drawn from [unseen_cards].
    pub fn determinized(
        &self,
        unseen_cards: &mut Vec<Card>,
        rng: &mut CambioRng
    ) -> FastJaggedVec<Card> {
        FastJaggedVec {
            cards: self.cards.iter()
                .map(|card|
                    card.determinized_if_unknown_from(unseen_cards, rng)
                )
                .collect(),
            player_start_indices: self.player_start_indices.clone()
        }
    }
}

/// Index with [CardPosition] to get a specific card.
impl<T: UnderlyingCardType + Copy> Index<CardPosition> for FastJaggedVec<T> {
    type Output = CardAndVisibility<T>;

    fn index(&self, position: CardPosition) -> &Self::Output {
        &self.cards[self.raw_index(position)]
    }
}

impl<T: UnderlyingCardType + Copy> IndexMut<CardPosition> for FastJaggedVec<T> {
    fn index_mut(&mut self, position: CardPosition) -> &mut Self::Output {
        // For some reason Rust won't let me move this into the brackets
        let idx = self.raw_index(position);
        &mut self.cards[idx]
    }
}

/// Index with a [Player] to get all of a player's cards.
impl<T: UnderlyingCardType> Index<Player> for FastJaggedVec<T> {
    type Output = [CardAndVisibility<T>];

    fn index(&self, player: Player) -> &Self::Output {
        let start = self.player_start_indices[player as usize];
        let end = self.player_start_indices[player as usize + 1];

        &self.cards[start..end]
    }
}

impl<T: UnderlyingCardType> IndexMut<Player> for FastJaggedVec<T> {
    fn index_mut(&mut self, player: Player) -> &mut Self::Output {
        let start = self.player_start_indices[player as usize];
        let end = self.player_start_indices[player as usize + 1];

        &mut self.cards[start..end]
    }
}