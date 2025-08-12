use std::collections::HashMap;
use std::rc::*;
use std::cell::*;
use rand::prelude::*;
use crate::cambio;

/// The exploration parameter for Monte Carlo tree search. Higher values will prioritize breadth
/// over depth.
const EXPLORATION_PARAMETER: f32 = 1.414;

/// Performs a Monte Carlo tree search and returns the win rates after performing each action.
pub fn search_from(
    game: &cambio::PartialInfoGame,
    num_playouts: usize,
    rng: &mut cambio::CambioRng
) -> SearchResults {
    let root = Rc::new(RefCell::new(
        Node::new(game, None, Weak::new())
    ));

    for _ in 0..num_playouts {
        Node::execute_playout(&root, game, rng);
    }

    let root_winrate = root.borrow().winrate();

    let actions: Vec<_> = root.borrow()
        .children.borrow().iter()
        .map(|(&action, child)|
            (action, child.borrow().winrate())
        )
        .collect();

    let mut non_stick_actions: Vec<_> = actions
        .iter().filter_map(|(action, winrate)|
            if let cambio::Action::Stick { .. } = action {
                None
            } else {
                Some((*action, *winrate))
            }
        )
        .collect();

    let mut sticks: Vec<_> = actions
        .iter().filter_map(|(action, winrate)|
            if let cambio::Action::Stick { .. } = action {
                Some((*action, *winrate))
            } else {
                None
            }
        )
        .collect();

    non_stick_actions.sort_by(|(_, a), (_, b)|
        f32::partial_cmp(b, a).unwrap()
    );
    sticks.sort_by(|(_, a), (_, b)|
        f32::partial_cmp(b, a).unwrap()
    );

    SearchResults {  root_winrate, non_stick_actions, sticks }
}

///
pub struct SearchResults {
    ///
    pub root_winrate: f32,

    ///
    pub non_stick_actions: Vec<(cambio::Action, f32)>,

    ///
    pub sticks: Vec<(cambio::Action, f32)>,
}

/// A node of a Monte Carlo tree.
///
/// This should always be wrapped in [Rc] as [Rc<Node>] because each node will have at
/// least one strong reference by its parent and weak references by its children.
#[derive(Debug)]
struct Node {
    /// The number of wins that [player] has earned from this position.
    wins: f32,

    /// The number of playouts that have been executed on this node.
    playouts: f32,

    /// The player who was responsible for the action that brought us to this move.
    ///
    /// If this is a root node, i.e. `parent == Weak::new()`, then this is equivalent to the current
    /// turn.
    player: cambio::Player,

    /// Child nodes.
    ///
    /// This needs to be wrapped in a [RefCell] because all [Node]s are wrapped in [Rc]
    /// which only lets us borrow a node immutably; this would be unworkable when we need to add a
    /// child.
    children: RefCell<
        HashMap<cambio::Action, Rc<RefCell<Self>>>
    >,

    /// The parent node, or `Weak::new()` if this is the root node.
    parent: Weak<RefCell<Self>>
}

impl Node {
    /// The winrate for [player] from this node.
    pub fn winrate(&self) -> f32 {
        self.wins / self.playouts
    }

    /// Constructs a new [Node] wrapped in [Rc].
    ///
    /// [previous_action] is necessary to determine the [player] field. Normally, this is the player
    /// to move, but if the previous move was a stick, it should be the player who stuck the card;
    /// if the previous move was [cambio::Action::CallCambio], it should be the previous player.
    fn new<T: cambio::UnderlyingCardType + Copy>(
        game: &cambio::Game<T>,
        previous_action: Option<cambio::Action>,
        parent: Weak<RefCell<Self>>
    ) -> Self {
        Self {
            wins: 0.,
            playouts: 0.,
            player: match previous_action {
                Some(cambio::Action::Stick { stick_player, .. }) =>
                    stick_player as cambio::Player,
                Some(cambio::Action::CallCambio) =>
                    game.prev_turn(),
                _ =>
                    game.turn()
            },
            children: RefCell::new(HashMap::new()),
            parent
        }
    }

    fn uct(&self) -> f32 {
        self.winrate()
            + EXPLORATION_PARAMETER * (
                self.parent.upgrade()
                    .expect("Cannot get UCT of Monte Carlo node without parent")
                    .borrow()
                    .playouts
                    .ln()
                / self.playouts
            ).sqrt()
    }

    fn execute_playout(
        node: &Rc<RefCell<Self>>,
        game: &cambio::PartialInfoGame,
        rng: &mut cambio::CambioRng
    ) {
        let mut det = cambio::DeterminizedGame::randomized_from(game, rng);
        let new_node = Self::select_and_expand(node, &mut det, rng);

        let winners = loop {
            // Try to randomly choose an action
            if let Some(&action) = det.legal_actions().choose(rng) {
                // Execute it
                det.execute(action, rng);
            } else {
                // If no legal actions, then return the winners
                break det.winners();
            }
        };

        Self::backprop(new_node, &winners, 1. / winners.len() as f32);
    }

    fn select_and_expand(
        node: &Rc<RefCell<Self>>,
        game: &mut cambio::DeterminizedGame,
        rng: &mut cambio::CambioRng
    ) -> Rc<RefCell<Self>> {
        let legal_actions = game.legal_actions();
        let unexpanded_legal_actions = legal_actions
            .iter()
            .filter(|action| !node.borrow()
                .children.borrow().contains_key(action)
            );
        let node_borrowed = node.borrow();

        if let Some(&unexpanded_action) = unexpanded_legal_actions.choose(rng) {
            /* There is an unexpanded legal action */

            // Execute the action
            game.execute(unexpanded_action, rng);

            // Create a new child node to represent it
            let expanded_node = Node::new(
                game,
                Some(unexpanded_action),
                Rc::downgrade(node)
            );

            // Add the node to [children]
            node_borrowed
                .children.borrow_mut()
                .insert(unexpanded_action, Rc::new(RefCell::new(expanded_node)));

            // Return the added node
            Rc::clone(
                &node_borrowed
                    .children.borrow()[&unexpanded_action]
            )
        } else {
            /*
            All legal actions from [game] have been expanded; select one and continue recursively
             */
            // Must be bound to a local variable so that the borrow remains valid in the scope
            let children = node_borrowed
                .children.borrow();

            // Get all children that are legal actions from [game]
            let legal_children: Vec<_> =
                children
                    .iter()
                    // Filter children that are legal actions
                    .filter(|(action, _)|
                        legal_actions.contains(action)
                    )
                    .collect();

            // No children, can't select a node; stop recursion and return self
            if legal_children.is_empty() {
                return Rc::clone(node);
            }

            // Find child with highest UCT. The code is wrapped in a block to prevent it being
            // mutated later.
            let (&action, child) = {
                // Start with the first child
                let mut max_uct = legal_children[0].1.borrow().uct();
                let mut max_uct_child = legal_children[0];

                // Look for children with higher UCTs
                for &child in &legal_children[1..] {
                    let new_uct = child.1.borrow().uct();
                    if new_uct > max_uct {
                        max_uct = new_uct;
                        max_uct_child = child;
                    }
                }
                max_uct_child
            };

            // Execute the action
            game.execute(action, rng);

            // Expand the child
            Node::select_and_expand(child, game, rng)
        }
    }

    fn backprop(node: Rc<RefCell<Self>>, winners: &Vec<cambio::Player>, credit: f32) {
        let mut node_borrowed = node.borrow_mut();

        node_borrowed.playouts += 1.0;

        if winners.contains(&node_borrowed.player) {
            node_borrowed.wins += credit;
        }

        if let Some(parent) = node_borrowed.parent.upgrade() {
            Self::backprop(parent, winners, credit);
        }
    }
}