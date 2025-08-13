use std::fmt::Display;
use std::str::FromStr;
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

/// The result of [Card::pick_known].
pub enum PickKnownCardResult {
    /// The two provided cards conflict.
    Conflicting(Card, Card),

    /// The card is still unknown.
    Unknown,

    /// The card is now known.
    Ok(Card)
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

    /// Pick the known card out of [a] and [b]. This is used in situations where a card might not be
    /// known and might also be given by the user, but we want to check if the card conflicts with
    /// what we thought the card was. See [PickKnownCardResult].
    ///
    /// If exactly one of [a] and [b] is [Some], that one is returned as [Ok].
    ///
    /// If either both or none of [a] and [b] are [Some], [Err] is returned.
    pub fn pick_known(a: Option<Card>, b: Option<Card>) -> PickKnownCardResult {
        if let Some(a) = a {
            if let Some(b) = b {
                if a == b {
                    PickKnownCardResult::Ok(a)
                } else {
                    PickKnownCardResult::Conflicting(a, b)
                }
            } else {
                PickKnownCardResult::Ok(a)
            }
        } else if let Some(b) = b {
            PickKnownCardResult::Ok(b)
        } else {
            PickKnownCardResult::Unknown
        }
    }
}

impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = String::from(match self {
            Card::Ace => "A",
            Card::Two => "2",
            Card::Three => "3",
            Card::Four => "4",
            Card::Five => "5",
            Card::Six => "6",
            Card::Seven => "7",
            Card::Eight => "8",
            Card::Nine => "9",
            Card::Ten => "10",
            Card::Jack => "J",
            Card::Queen => "Q",
            Card::BlackKing => "bK",
            Card::RedKing => "rK",
            Card::Joker => "Joker",
        });
        write!(f, "{}", str)
    }
}

impl FromStr for Card {
    type Err = clap::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim().to_uppercase().as_str() {
            "A" | "1" => Ok(Card::Ace),
            "2" => Ok(Card::Two),
            "3" => Ok(Card::Three),
            "4" => Ok(Card::Four),
            "5" => Ok(Card::Five),
            "6" => Ok(Card::Six),
            "7" => Ok(Card::Seven),
            "8" => Ok(Card::Eight),
            "9" => Ok(Card::Nine),
            "X" | "10" => Ok(Card::Ten),
            "J" => Ok(Card::Jack),
            "Q" => Ok(Card::Queen),
            "B" | "BK" => Ok(Card::BlackKing),
            "R" | "RK" | "-1" => Ok(Card::RedKing),
            "0" | "JOKER" => Ok(Card::Joker),
            _ => Err(clap::Error::raw(clap::error::ErrorKind::ValueValidation, "Not a card type")),
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
    pub value: UnderlyingCard,

    /// Bitflags whose indices signify players who have seen this card.
    seen_by_players: u32,
}

impl<UnderlyingCard: UnderlyingCardType + Copy> CardAndVisibility<UnderlyingCard> {
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

/// The location of a card in someone's pile, identified by its [player] and [index].
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct CardLocation {
    /// The player to whom the card identified by this [CardLocation] belongs to.
    pub player: u8,
    /// The index of the card within [player]'s cards that this [CardLocation] identifies.
    pub index: u8
}

impl CardLocation {
    /// Initializes a [CardLocation] while getting rid of the boilerplate code to cast [player] and
    /// [index] to the correct types.
    pub fn new(player: usize, index: usize) -> Self {
        Self {
            player: player as u8,
            index: index as u8
        }
    }
}

impl Display for CardLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "P{} #{}", self.player, self.index)
    }
}

impl FromStr for CardLocation {
    type Err = clap::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<&str> = s
            .split('.')
            .collect();

        if coords.len() != 2 {
            return Err(clap::Error::raw(clap::error::ErrorKind::ValueValidation, "Too many numbers in card location"));
        }

        let player: Result<u8, _> =
            coords[0].parse();
        let index: Result<u8, _> =
            coords[1].parse();

        if let (Ok(player), Ok(index)) = (player, index) {
            Ok(Self { player, index })
        } else {
            Err(clap::Error::raw(clap::error::ErrorKind::ValueValidation, "Not a card location"))
        }
    }
}