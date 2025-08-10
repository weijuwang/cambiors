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
#[derive(Copy, Clone, Debug)]
#[repr(u8)]
enum State<UnderlyingCard: UnderlyingCardType> {
    BeginningOfTurn,
    AfterDrawing(UnderlyingCard),
    AfterDiscard7Or8,
    AfterDiscard9Or10,
    AfterDiscardFace,
    AfterDiscardBlackKing,
    AfterBlackKingPeeked(CardPosition),
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
    Swap(CardPosition),

    /// Swap any two cards from different players.
    ///
    /// If switching cards after discarding a black king, the first card is always the one that was
    /// looked at.
    BlindSwitch(CardPosition, CardPosition),

    /// Peek at a card.
    Peek(CardPosition),

    /// Stick a card without giving away a card, i.e. sticking one's own card. This can also be used
    /// when you stick someone else's card and don't want to give away a card.
    StickWithoutGiveAway(CardPosition),

    /// Give away a card after sticking someone else's card, i.e. sticking someone else's card.
    ///
    /// The player sticking and giving away the card is the same.
    StickWithGiveAway { stick_position: CardPosition, give_away_position: CardPosition },

    /// Call "Cambio," beginning the endgame.
    CallCambio,

    /// Skip an optional action.
    SkipOptional,

    /// Pass the turn to the next player, prohibiting anyone from sticking.
    EndTurn
}