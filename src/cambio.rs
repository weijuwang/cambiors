use std::fmt::Display;
use rand_xoshiro::Xoshiro256PlusPlus;
use rand::prelude::*;

/// Structs and methods relating to cards in Cambio, ways to store related information such as who
/// has seen a card and the location of a card.
pub mod card;
pub use card::*;

/// Structs and methods representing the entire state of a Cambio game
pub mod game;
pub use game::*;

/// A fast two-dimensional vector of cards used to store information about what cards players have.
///
/// See [FastJaggedVec].
mod fast_jagged_vec;

/// The index of a player in the game.
pub type Player = usize;

/// The random number generator used for this module. It is important that this be as fast as
/// possible because generating random numbers or shuffling the deck is often a bottleneck.
pub type CambioRng = Xoshiro256PlusPlus;

/// Removes a random item from a [Vec].
fn remove_random_from<T>(v: &mut Vec<T>, rng: &mut CambioRng) -> T {
    v.remove(
        rng.random_range(0..v.len())
    )
}

/// The state of a [Game]. This tells us which moves are currently legal.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[repr(u8)]
pub enum State<UnderlyingCard: UnderlyingCardType> {
    BeginningOfTurn,
    AfterDrawing(UnderlyingCard),
    AfterDiscard7Or8,
    AfterDiscard9Or10,
    AfterDiscardFace,
    AfterDiscardBlackKing,
    AfterBlackKingPeeked(CardLocation),
    EndOfTurn,
    EndOfGame
}

/// An action that can be executed on a [Game].
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum Action {
    /// Draw a card.
    Draw,

    /// Discard the most recently discarded card.
    Discard,

    /// Swap the card just drawn with a card in the player's own pile.
    SwapDrawnCardForOwn(CardLocation),

    /// Swap any two cards from different players.
    ///
    /// If switching cards after discarding a black king, the first card is always the one that was
    /// looked at.
    BlindSwitch(CardLocation, CardLocation),

    /// Peek at a card.
    Peek(CardLocation),

    /// Stick a card.
    Stick {
        stick_player: Player,
        stick_location: CardLocation,
        give_away_location: Option<CardLocation>
    },

    /// Call "Cambio," beginning the endgame.
    CallCambio,

    /// Skip an optional action.
    SkipOptional,

    /// Pass the turn to the next player, prohibiting anyone from sticking.
    EndTurn
}

impl Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Draw =>
                write!(f, "Draw"),
            Action::Discard =>
                write!(f, "Discard"),
            Action::SwapDrawnCardForOwn(pos) =>
                write!(f, "Swap for {pos}"),
            Action::BlindSwitch(a, b) =>
                write!(f, "Switch {a} and {b}"),
            Action::Peek(pos) =>
                write!(f, "Peek {pos}"),
            Action::Stick { stick_player, stick_location, give_away_location } =>
                write!(f, "Stick  {stick_location} by {stick_player}{}",
                       if let Some(give_away_location) = give_away_location {
                        format!(", give-away {give_away_location}")
                    } else {
                        String::new()
                    }
                ),
            Action::CallCambio =>
                write!(f, "Call Cambio"),
            Action::SkipOptional =>
                write!(f, "Skip"),
            Action::EndTurn =>
                write!(f, "End turn"),
        }
    }
}