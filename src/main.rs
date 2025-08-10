extern crate core;

/// An API for Cambio.
mod cambio;

/// Monte Carlo tree search.
mod mcts;

use std::time::Instant;
use rand::prelude::*;
use rand_xoshiro::Xoshiro256PlusPlus;

fn main() {
    let mut rng = Xoshiro256PlusPlus::from_rng(&mut rand::rng());
    let pig = cambio::PartialInfoGame::new(3, 0, cambio::Card::RedKing, cambio::Card::RedKing);

    let n = 200_000;

    let start = Instant::now();
    let search_results = mcts::search_from(&pig, n, &mut rng);
    let micros = start.elapsed().as_micros() as f64;

    println!("{} us", micros / n as f64);
}
