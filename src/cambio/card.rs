use super::*;

/// Card types that are considered unique within the game of Cambio.
///
/// For aces or cards from 2 to 10 inclusive, use [Number]. Otherwise, use the appropriately named
/// enum: [Jack], [Queen], [BlackKing], [RedKing], or [Joker].
///
/// Each card is listed out individually to save memory in the representation.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Card {
    Ace, Two, Three, Four, Five,
    Six, Seven, Eight, Nine, Ten,
    Jack,
    Queen,
    BlackKing,
    RedKing,
    Joker
}

impl Card {
    /// The number of points this card is worth.
    pub fn points(&self) -> i32 {
        match self {
            Card::Ace => 1,
            Card::Two => 2,
            Card::Three => 3,
            Card::Four => 4,
            Card::Five => 5,
            Card::Six => 6,
            Card::Seven => 7,
            Card::Eight => 8,
            Card::Nine => 9,
            Card::Ten => 10,
            Card::Jack | Card::Queen | Card::BlackKing => 10,
            Card::RedKing => -1,
            Card::Joker => 0,
        }
    }
}

/// Either [Card] or [Option<Card>], depending on whether the context is a [DeterminizedGame] or
/// [PartialInfoGame].
pub trait UnderlyingCardType {}
impl UnderlyingCardType for Card {}
impl UnderlyingCardType for Option<Card> {}

/// Information on a card as well as who has seen this card ([visible_to]).
#[derive(Copy, Clone)]
pub struct CardAndVisibility<UnderlyingCard: UnderlyingCardType> {
    /// The actual card represented by this struct.
    value: UnderlyingCard,

    /// Bitflags whose indices signify players who have seen this card.
    seen_by_players: u32,
}

impl<UnderlyingCard: UnderlyingCardType + Copy> CardAndVisibility<UnderlyingCard> {
    pub fn value(&self) -> UnderlyingCard {
        self.value
    }

    /// Create a new card that has been seen by no players.
    pub fn new_seen_by_nobody(value: UnderlyingCard) -> Self {
        Self {
            value,
            seen_by_players: 0
        }
    }

    /// Create a new card that has only been seen by one player.
    pub fn new_seen_by_one(value: UnderlyingCard, visible_to: Player) -> Self {
        Self {
            value,
            seen_by_players: 1 << visible_to
        }
    }

    /// Record that [player] has seen this card.
    pub fn show_to(&mut self, player: Player) {
        self.seen_by_players |= 1 << player;
    }

    /// Check whether [player] has seen this card.
    pub fn seen_by(&self, player: Player) -> bool {
        self.seen_by_players & (1 << player) != 0
    }
}

impl CardAndVisibility<Option<Card>> {
    /// Creates a new version of this [CardAndVisibility] with the card drawn from [unseen_cards] if
    /// unknown.
    pub fn determinized_if_unknown_from(
        &self,
        unseen_cards: &mut Vec<Card>,
        rng: &mut CambioRng
    ) -> CardAndVisibility<Card> {
        CardAndVisibility {
            value: match self.value {
                Some(card) => {
                    card
                }
                None => {
                    remove_random_from(unseen_cards, rng)
                }
            },
            seen_by_players: self.seen_by_players
        }
    }
}

/// The position of a card in someone's pile, identified by its [player] and [index].
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct CardPosition {
    /// The player to whom the card identified by this [CardPosition] belongs to.
    pub player: u8,
    /// The index of the card within [player]'s cards that this [CardPosition] identifies.
    pub index: u8
}