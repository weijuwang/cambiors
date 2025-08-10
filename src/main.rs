/// An API for Cambio.
mod cambio;

/// Monte Carlo tree search.
mod mcts;

use std::io;
use std::io::Write;
use std::time::Instant;
use rand::prelude::*;

/// The message displayed at the start of the game.
const START_MESSAGE: &str = "Welcome to cambiors!\nLast updated 10 Aug 2025.\n";

const REPL_PROMPT: &str = ">";

/// Reads a line from stdin. If the line doesn't read for some reason, it will keep trying until
/// it is successful.
fn read_line(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush()
        .expect("Couldn't print prompt");

    let mut string = String::new();
    loop {
        if io::stdin()
            .read_line(&mut string)
            .is_ok()
        {
            break string
        }
    }
}

/// Reads a [usize] from stdin. If the line doesn't read for some reason, it will keep trying until
/// it is successful.
fn read_usize(prompt: &str) -> usize {
    let raw_input = read_line(prompt);
    loop {
        if let Ok(result) = raw_input.trim().parse::<usize>() {
            break result
        }
    }
}

/// Prints a stylized log message with a period at the end.
fn log(message: &str) {
    println!("[{}.]", message);
}

fn main() {
    println!("{}", START_MESSAGE);

    let mut rng = cambio::CambioRng::from_rng(&mut rand::rng());
    log("Initialized RNG");

    let game = cambio::PartialInfoGame::new(2, 0, true, cambio::Card::Joker, cambio::Card::Ten);
    log("Initialized partial-info game");

    let mut num_playouts = 100_000;

    loop {
        let raw_input = read_line(REPL_PROMPT);
        let args: Vec<_> = raw_input
            .split_whitespace()
            .collect();

        if args.is_empty() {
            continue
        }

        /* PLayouts */

        println!("Turn {}/{}", game.turn(), game.num_players());

        let start = Instant::now();
        let search_results = mcts::search_from(&game, num_playouts, &mut rng);
        let micros = start.elapsed().as_micros() as f64;

        for (action, winrate) in search_results {
            println!("{:?}: {}%", action, (winrate * 1000.).round() / 10.);
        }

        println!("{} us/playout", micros / num_playouts as f64);

        match args[0] {

            "exit" => {
                println!("Exiting.");
                break;
            }

            _ => {
                println!("Unrecognized command. No action taken.");
            }
        }
    }

    log("Exited cambiors");
}
