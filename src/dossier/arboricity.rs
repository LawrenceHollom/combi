use utilities::edge_tools::*;

use crate::entity::graph::*;

fn maker_wins_arboricity_game_rec(g: &Graph, indexer: &EdgeIndexer, k: usize, max_colour_used: usize, 
        colours: &mut EdgeVec<Option<usize>>, num_edges: usize, num_cold: usize) -> bool {
    // Recursively try to colour an edge.
    if num_cold >= num_edges {
        // maker wins as all edges are coloured.
        true
    } else {
        // Colour another edge if possible.
        let mut is_something_playable = false;
        let maker_turn = num_cold % 2 == 0;
        let mut maker_win = !maker_turn;
        let col_cap = (max_colour_used + 2).min(k);
        'test_edges: for e in indexer.iter_edges() {
            if colours[*e].is_none() {
                let mut can_be_cold = false;
                for c in 0..col_cap {
                    // Try colouring the edge c and see if a monox cycle is made.
                    // filtered acyclic.
                    colours[*e] = Some(c);
                    let mut filter = EdgeSet::new(indexer);
                    for f in indexer.iter_edges() {
                        if colours[*f] == Some(c) {
                            filter.add_edge(*f, indexer);
                        }
                    }
                    if g.is_filtered_acyclic(filter, indexer) {
                        let max_col = max_colour_used.max(c);
                        can_be_cold = true;
                        is_something_playable = true;
                        let sub_maker_win = maker_wins_arboricity_game_rec(g, indexer, k, 
                            max_col, colours, num_edges, num_cold + 1);
                        if sub_maker_win == maker_turn {
                            // This is a winning strategy for the current player.
                            maker_win = sub_maker_win;
                            colours[*e] = None;
                            break 'test_edges;
                        }
                    }
                    colours[*e] = None;
                }
                if !can_be_cold {
                    // This edge cannot be coloured and so breaker wins
                    maker_win = false;
                    break 'test_edges;
                }
            }
        }
        if !is_something_playable {
            // Nothing is playable, so breaker wins.
            maker_win = false;
        }
        maker_win
    }
}

pub fn maker_wins_arboricity_game(g: &Graph, k: usize) -> bool {
    let indexer = EdgeIndexer::new(&g.adj_list);
    let mut maker_wins = false;
    let mut colours = EdgeVec::new(&g.adj_list, None);
    'test_edges: for e in indexer.iter_edges() {
        colours[*e] = Some(0);
        if maker_wins_arboricity_game_rec(g, &indexer, k, 0, &mut colours,
                g.size(), 1) {
            maker_wins = true;
            break 'test_edges;
        }
        colours[*e] = None;
    }
    maker_wins
}

pub fn game_arboricity_number(g: &Graph) -> u32 {
    let mut k = 1;
    if g.size() == 0 {
        return 0;
    }
    loop {
        if maker_wins_arboricity_game(g, k) {
            return k as u32;
        }
        k += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::constructor::RawConstructor;

    use super::*;
    use utilities::*;
    use crate::constructor::*;

    fn test_arb_game(g: &Graph, a_g: u32) {
        assert_eq!(game_arboricity_number(g), a_g)
    }

    fn ous(n: usize) -> Order {
        Order::of_usize(n)
    }

    #[test]
    fn test_arb_game_prism() {
        use Constructor::*;
        let constr = Product(ProductConstructor::Cartesian, 
            Box::new(Raw(RawConstructor::Complete(ous(2)))), 
            Box::new(Raw(RawConstructor::Complete(ous(3)))));
        test_arb_game(&constr.new_entity().as_owned_graph(), 2)
    }

    #[test]
    fn test_arb_game_k33() {
        test_arb_game(&Graph::new_complete_bipartite(ous(3), ous(3)), 2);
    }

    #[test]
    fn test_arb_game_k4() {
        test_arb_game(&Graph::new_complete(ous(4)), 3);
    }

    #[test]
    fn test_arb_game_e10() {
        test_arb_game(&Graph::new_empty(ous(10)), 0);
    }

    #[test]
    fn test_arb_game_p4() {
        test_arb_game(&Graph::new_path(Order::of_usize(4)), 1);
    }
}
