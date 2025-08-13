
/// An API for Cambio.
mod cambio;

/// Monte Carlo tree search.
mod mcts;

use std::time::Instant;
use std::str::FromStr;
use rand::SeedableRng;
use clap::*;
use reedline_repl_rs::*;

const PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");

const DESCRIPTION: &str = "Command-line interface for a partial-information Cambio card game API and Monte Carlo tree search.";

/// The banner that is displayed at start-up.
const BANNER: &str = "Welcome to cambiors! Last updated 12 Aug 2025.\nPress Ctrl+D to exit. Players and cards are zero-indexed.";

/// The default number of playouts in Monte Carlo tree search.
const DEFAULT_PLAYOUTS: usize = 100_000;

#[derive(Parser)]
#[command(name = PROGRAM_NAME, author = "Weiju Wang")]
#[command(about = DESCRIPTION)]
struct Args {
    /// Number of players in the game
    #[arg(short, long)]
    num_players: cambio::Player,

    /// First player
    #[arg(short, long)]
    first_player: cambio::Player,

    /// Whether there are jokers in the game
    #[arg(short, long, default_value_t = true)]
    jokers: bool,

    /// Player 0's bottom left card
    #[arg(short = 'l', long = "bottom-left")]
    #[arg(value_parser = cambio::Card::from_str)]
    bottom_left_card: cambio::Card,

    /// Player 0's bottom right card
    #[arg(short = 'r', long = "bottom-right")]
    #[arg(value_parser = cambio::Card::from_str)]
    bottom_right_card: cambio::Card,
}

/// Context variables passed to the REPL.
struct Context {
    /// The random number generator used in this program.
    rng: cambio::CambioRng,

    /// The game object manipulated by the REPL.
    game: cambio::PartialInfoGame,

    /// The number of playouts in Monte Carlo tree search.
    num_playouts: usize,

    /// The action that will be executed on [game] before prompting for the next command.
    next_action: Option<(cambio::Action, Option<cambio::Card>)>,

    /// The player who had the turn at the beginning of the game.
    first_player: cambio::Player,

    /// The list of actions that have been executed in this session.
    action_history: Vec<cambio::Action>
}

fn game_as_string(context: &Context) -> String {
    let top_line = format!(
        "Turn {}/{}  {} to draw  {} discarded",
        context.game.turn(),
        context.game.num_players(),
        context.game.draw_deck_size(),
        context.game.discard_pile().len()
    );

    let draw_or_discard =
        if let cambio::State::AfterDrawing(card) = context.game.state() {
            if let Some(card) = card {
                format!("  Just drew {card}")
            } else {
                "  Just drew ?".to_string()
            }
        } else if let Some(last_discarded) = context.game.discard_pile().last() {
            format!("  Last discarded {last_discarded}")
        } else {
            String::new()
        };

    let cambio_call =
        if let Some(cambio_caller) = context.game.cambio_caller() {
            format!("  Cambio called by {cambio_caller}")
        } else {
            String::new()
        };

    let stick =
        if context.game.already_stuck() {
            "  Can't stick".to_string()
        } else {
            String::new()
        };
    format!(
        "{}{}{}{} ",
        top_line,
        draw_or_discard,
        cambio_call,
        stick
    )
}

fn game_cards(context: &Context) -> String {
    (0..context.game.num_players())
        .map(|player|
            paint_yellow_bold(&player.to_string()) + " " +
                &*context.game.player_cards(player)
                    .iter()
                    .map(|card|
                        if let Some(value) = card.value {
                            value.to_string()
                        } else {
                            "?".to_string()
                        }
                    )
                    .collect::<Vec<String>>()
                    .join(" ")
        )
        .collect::<Vec<String>>()
        .join("\n")
}

fn main() -> Result<()> {
    let cmd_args = Args::parse();
    let context = Context {
        rng: cambio::CambioRng::from_rng(&mut rand::rng()),
        game: cambio::PartialInfoGame::new(
            cmd_args.num_players,
            cmd_args.first_player,
            cmd_args.jokers,
            cmd_args.bottom_left_card,
            cmd_args.bottom_right_card
        ),
        num_playouts: DEFAULT_PLAYOUTS,
        next_action: None,
        first_player: cmd_args.first_player,
        action_history: vec![]
    };

    Repl::<_, reedline_repl_rs::Error>::new(context)
        .with_name(PROGRAM_NAME)
        .with_description(DESCRIPTION)
        .with_banner(BANNER)
        .with_on_after_command(|context| {
            if let Some((action, card)) = context.next_action
            {
                match context.game.execute(action, card) {
                    Ok(()) => {
                        context.action_history.push(action);
                    }
                    Err(error) => {
                        println!("  Can't execute action: {}", error);
                    }
                }
            }
            context.next_action = None;
            Ok(Some(game_as_string(context)))
        })

        // TODO Manual

        .with_command(
            Command::new("cards")
                .about("Displays the cards that each player has."),
            |_, context| {
                Ok(Some(game_cards(context)))
            })

        .with_command(
            Command::new("game")
                .about("Displays the current game state."),
            |_, context| {
                Ok(Some(game_as_string(context) + "\n" + &*game_cards(context)))
            })

        .with_command(
            Command::new("set")
                .about("Modifies app settings for this session.")
                .arg(
                    Arg::new("playouts")
                        .long("playouts")
                        .help("The number of Monte Carlo playouts to execute for each search.")
                        .required(false)
                        .value_parser(value_parser!(usize))
                )
                .aliases(["rollouts"]),
            |args, context| {
                if let Some(new_num) = args.get_one("playouts") {
                    context.num_playouts = *new_num;
                };
                Ok(None)
            }
        )

        .with_command(
            Command::new("search")
                .about("Performs a Monte Carlo tree search.")
                .aliases(["mcts", "montecarlo"]),
            |_, context| {
                let start = Instant::now();
                match mcts::search_from(&context.game, context.num_playouts, &mut context.rng) {
                    Ok(search_results) => {
                        let micros = start.elapsed().as_micros() as f32;

                        println!(
                            "{} ({} playouts, {} us/playout)",
                            paint_yellow_bold("Search results"),
                            context.num_playouts,
                            micros / context.num_playouts as f32
                        );
                        println!(
                            "  {}: {}%",
                            paint_yellow_bold("From root"),
                            (search_results.root_winrate * 100.).round()
                        );
                        for (action, winrate) in search_results.non_stick_actions {
                            println!("    {action}: {}%", (winrate * 100.).round());
                        }
                        println!("  {}", paint_yellow_bold("Sticks"));
                        for (action, winrate) in search_results.sticks {
                            println!("    {action}: {}%", (winrate * 100.).round());
                        }
                    }
                    Err(reason) => {
                        println!("Can't search: {reason}")
                    }
                }

                Ok(None)
            }
        )

        .with_command(
            Command::new("history")
                .about("Displays the list of actions that have been executed so far."),
            |_, context| {
                let mut player = context.first_player;
                let mut new_player = true;

                for action in context.action_history.iter() {
                    if new_player {
                        player += 1;
                        println!("{}", paint_yellow_bold(&format!("{}", player % context.game.num_players())));
                        new_player = false;
                    }

                    match action {
                        cambio::Action::EndTurn => {
                            new_player = true;
                        }
                        cambio::Action::CallCambio => {
                            println!("  {}", paint_yellow_bold("Call Cambio"));
                            new_player = true;
                        }
                        cambio::Action::Stick { .. } => {
                            println!("  {}", paint_yellow_bold(&format!("{}", action)));
                        }
                        _ => {
                            println!("  {action}",);
                        }
                    }
                }
                Ok(None)
            }
        )

        .with_command(
            Command::new("draw")
                .about("Draws a card.")
                .arg(
                    Arg::new("drawn-card")
                        .required(false)
                        .help("The card drawn if known.")
                        .value_parser(value_parser!(cambio::Card))
                ),
            |args, context| {
                // [Some] if a drawn card was provided or [None] otherwise
                let drawn_card = args.get_one("drawn-card").copied();
                context.next_action = Some((cambio::Action::Draw, drawn_card));
                Ok(None)
            }
        )

        .with_command(
            Command::new("discard")
                .about("Discards the card that was just drawn.")
                .arg(
                    Arg::new("discarded-card")
                        .required(false)
                        .help("The card discarded if known.")
                        .value_parser(value_parser!(cambio::Card))
                ),
            |args, context| {
                let discarded_card = args.get_one("discarded-card").copied();
                context.next_action = Some((cambio::Action::Discard, discarded_card));
                Ok(None)
            }
        )

        .with_command(
            Command::new("swap")
                .about("Swaps the card that was just drawn with a card from the same player.")
                .arg(
                    Arg::new("index-to-replace")
                        .help("The index of the card that was discarded.")
                        .required(true)
                        .value_parser(value_parser!(usize))
                )
                .arg(
                    Arg::new("replaced-card")
                        .help("The card replaced and discarded if known.")
                        .required(false)
                        .value_parser(value_parser!(cambio::Card))
                ),
            |args, context| {
                let index_to_replace = *args.get_one("index-to-replace").unwrap();
                let replaced_card = args.get_one("replaced-card").copied();

                let position_to_replace = cambio::CardPosition::new(
                    context.game.turn(), index_to_replace
                );

                context.next_action = Some((
                    cambio::Action::SwapDrawnCardForOwn(position_to_replace),
                    replaced_card
                ));
                Ok(None)
            }
        )

        .with_command(
            Command::new("switch")
                .about("Switches two players' cards.")
                .arg(
                    Arg::new("position-a")
                        .help("The position of the first card to swap.")
                        .required(true)
                        .value_parser(value_parser!(cambio::CardPosition))
                )
                .arg(
                    Arg::new("position-b")
                        .help("The position of the second card to swap.")
                        .required(true)
                        .value_parser(value_parser!(cambio::CardPosition)),
                ),
            |args, context| {
                let pos_a = *args.get_one("position-a").unwrap();
                let pos_b = *args.get_one("position-b").unwrap();
                context.next_action = Some((cambio::Action::BlindSwitch(pos_a, pos_b), None));
                Ok(None)
            }
        )

        .with_command(
            Command::new("peek")
                .about("Records that the player whose turn it is peeked at a card.")
                .arg(
                    Arg::new("position")
                        .help("The position of the card peeked.")
                        .required(true)
                        .value_parser(value_parser!(cambio::CardPosition)),
                )
                .arg(
                    Arg::new("peeked-card")
                        .help("The card peeked at if known.")
                        .required(false)
                        .value_parser(value_parser!(cambio::Card))
                ),
            |args, context| {
                let position = *args.get_one("position").unwrap();
                let peeked_card = args.get_one("peeked-card").copied();
                context.next_action = Some((cambio::Action::Peek(position), peeked_card));
                Ok(None)
            }
        )

        .with_command(
            Command::new("stick")
                .about("Sticks a card.")
                .arg(
                    Arg::new("stick-player")
                        .help("The player sticking the card.")
                        .required(true)
                        .value_parser(value_parser!(cambio::Player))
                )
                .arg(
                    Arg::new("stick-position")
                        .help("The position of the card to stick.")
                        .required(true)
                        .value_parser(value_parser!(cambio::CardPosition))
                )
                .arg(
                    Arg::new("give-away-position")
                        .help("The position of the card to give away.")
                        .required(false)
                        .value_parser(value_parser!(cambio::CardPosition))
                ),
            |args, context| {
                let stick_player = *args.get_one("stick-player").unwrap();
                let stick_position = *args.get_one("stick-position").unwrap();
                let give_away_position = args.get_one("give-away-position").copied();
                context.next_action = Some((cambio::Action::Stick { stick_player, stick_position, give_away_position }, None));
                Ok(None)
            }
        )

        .with_command(
            Command::new("call-cambio")
                .about("Calls Cambio, beginning the endgame."),
            |_, context| {
                context.next_action = Some((cambio::Action::CallCambio, None));
                Ok(None)
            }
        )

        .with_command(
            Command::new("skip")
                .about("Skips an optional action."),
            |_, context| {
                context.next_action = Some((cambio::Action::SkipOptional, None));
                Ok(None)
            }
        )

        .with_command(
            Command::new("end-turn")
                .about("Ends this turn."),
            |_, context| {
                context.next_action = Some((cambio::Action::EndTurn, None));
                Ok(None)
            }
        )

        .run()
}
