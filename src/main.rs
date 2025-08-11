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
    let mut string = String::new();
    loop {
        print!("{} ", prompt);
        io::stdout().flush()
            .expect("Couldn't print prompt");
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
    loop {
        let raw_input = read_line(prompt);
        if let Ok(result) = raw_input.trim().parse::<usize>() {
            break result
        } else {
            println!("Please enter an integer!");
        }
    }
}

/// Prints a stylized log message with a period at the end.
fn log(message: &str) {
    println!("[{}.]", message);
}

fn main() {
    println!("{}", START_MESSAGE);

    let num_players = read_usize("How many players?");
    let first_player = read_usize("First player?");

    let mut rng = cambio::CambioRng::from_rng(&mut rand::rng());
    log("Initialized RNG");

    let mut game = cambio::PartialInfoGame::new(num_players, first_player, true, cambio::Card::Joker, cambio::Card::Ten);
    log("Initialized partial-info game");

    let mut num_playouts = 100_000;
    let mut run_search = true;

    loop {
        // TODO Really hacky code this hurts my eyes i need to fix this
        /* Print the game state */
        println!();
        println!("Turn {}/{}. {} to draw. {} discarded.", game.turn(), game.num_players(), game.draw_pile_size(), game.discard_pile().len());
        if let cambio::State::AfterDrawing(card) = game.state() {
            println!("Just drew ");
            if let Some(card) = card {
                print!("{}", card);
            }
        } else if let Some(last_discarded) = game.discard_pile().last() {
            println!("Last discarded was {}", last_discarded);
        }
        if let Some(cambio_caller) = game.cambio_caller() {
            print!("Cambio called by {}", cambio_caller);
        }
        if game.already_stuck() {
            println!("Can't stick");
        }
        println!("Cards");
        for i in 0..game.num_players() {
            print!("{}: ", i);
            for card in game.player_cards(i) {
                if let Some(value) = card.value() {
                    print!("{} ", value);
                } else {
                    print!("u ");
                }
            }
            println!();
        }

        /* Monte Carlo search */
        if run_search {
            log("Starting Monte Carlo search");
            let start = Instant::now();
            let search_results = mcts::search_from(&game, num_playouts, &mut rng);
            let micros = start.elapsed().as_micros() as f32;

            println!("Search results ({} playouts, {} us/playout)", num_playouts, micros / num_playouts as f32);
            for (action, winrate) in search_results {
                println!("  {}: {}%", action, (winrate * 100.).round());
            }
        }

        /* Read input */
        let raw_input = read_line(REPL_PROMPT);
        let args: Vec<_> = raw_input
            .split_whitespace()
            .collect();

        if args.is_empty() {
            run_search = false;
            continue
        }

        // For most commands we want to search again after running it
        run_search = true;

        let (action, revealed_card): (cambio::Action, Option<cambio::Card>) = match args[0] {
            "exit" if args.len() == 1 =>
                break,

            "playouts" if args.len() == 2 => {
                if let Ok(parsed_num) = args[1].parse() {
                    num_playouts = parsed_num;
                } else {
                    println!("Invalid number.");
                }

                run_search = false;
                continue;
            }

            "search" if args.len() == 1 => {
                continue;
            }

            // Draw
            "d" | "+" | "draw"
            if args.len() <= 2 && game.state() == cambio::State::BeginningOfTurn
            => {
                let card = if args.len() == 2 {
                    if let Ok(card) = args[1].parse::<cambio::Card>() {
                        Some(card)
                    } else {
                        println!("Invalid card.");
                        continue;
                    }
                } else {
                    None
                };

                (cambio::Action::Draw, card)
            }

            // Discard
            "d" | "-" | "discard"
            if args.len() == 2 || args.len() == 3 => {
                if args.len() == 2 {
                    if let Ok(card) = args[1].parse::<cambio::Card>() {
                        (cambio::Action::Discard, Some(card))
                    } else {
                        println!("Invalid card.");
                        continue;
                    }
                } else {
                    (cambio::Action::Discard, None)
                }
            }

            // Swap
            "x" | "t" | "s" | "swap" | "switch" | "trade"
            if args.len() == 2 || args.len() == 3 && matches!(game.state(), cambio::State::AfterDrawing(_)) => {
                if args.len() >= 2 {
                    if let Ok(pos) = args[1].parse::<cambio::CardPosition>() {
                        if args.len() == 3 {
                            if let Ok(card) = args[2].parse::<cambio::Card>() {
                                (cambio::Action::SwapDrawnCardForOwn(pos), Some(card))
                            } else {
                                println!("Invalid card.");
                                continue;
                            }
                        } else {
                            (cambio::Action::SwapDrawnCardForOwn(pos), None)
                        }
                    } else {
                        println!("Invalid position.");
                        continue;
                    }
                } else {
                    println!("No args.");
                    continue;
                }
            }

            // Blind switch
            "x" | "t" | "swap" | "switch" | "trade" | "y"
            if args.len() == 3 => {
                if let Ok(pos_a) = args[1].parse::<cambio::CardPosition>()
                    && let Ok(pos_b) = args[2].parse::<cambio::CardPosition>()
                {
                    (cambio::Action::BlindSwitch(pos_a, pos_b), None)
                } else {
                    println!("Invalid position(s).");
                    continue;
                }
            }

            // Peek
            "p" | "peek" | "y"
            if args.len() == 2 || args.len() == 3 => {
                let position = if let Ok(pos) = args[1].parse::<cambio::CardPosition>() {
                    pos
                } else {
                    println!("Invalid position.");
                    continue;
                };

                if args.len() == 3 {
                    if let Ok(card) = args[2].parse::<cambio::Card>() {
                        (cambio::Action::Peek(position), Some(card))
                    } else {
                        println!("Invalid card");
                        continue;
                    }
                } else {
                    (cambio::Action::Peek(position), None)
                }
            }

            // Stick
            "!" | "stick"
            if args.len() == 2 || args.len() == 3 => {
                if let Ok(stick_position) = args[1].parse::<cambio::CardPosition>() {
                    if args.len() == 3 {
                        if let Ok(give_away_position) = args[2].parse::<cambio::CardPosition>() {
                            (cambio::Action::StickWithGiveAway { stick_position, give_away_position }, None)
                        } else {
                            println!("Invalid give-away position.");
                            continue;
                        }
                    } else {
                        (cambio::Action::StickWithoutGiveAway(stick_position), None)
                    }
                } else {
                    println!("Invalid stick position.");
                    continue;
                }
            }

            // Cambio
            "c" | "cambio" if args.len() == 1 =>
                (cambio::Action::CallCambio, None),

            // Skip
            "n" | "no" | "skip" if args.len() == 1 =>
                (cambio::Action::SkipOptional, None),

            // End turn
            "next" if args.len() == 1 =>
                (cambio::Action::EndTurn, None),

            _ => {
                println!("Invalid action or command. No action taken.");
                run_search = false;
                continue
            }
        };

        if game.execute(action, revealed_card).is_ok() {
            log("Executed");
        } else {
            println!("Illegal action.");
            run_search = false;
        }
    }

    log("Exited cambiors");
}
