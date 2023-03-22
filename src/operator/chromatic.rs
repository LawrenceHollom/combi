use crate::graph::*;

use utilities::vertex_tools::*;

fn can_be_coloured_rec(g: &Graph, num_colours: usize, max_colour_used: usize, colour: &mut VertexVec<Option<usize>>, next_vert: Vertex) -> bool {
    if next_vert.is_n(g.n) {
        true
    } else {
        let mut can_colour = false;
        'test_all_colourings: for col in 0..usize::min(max_colour_used + 2, num_colours) {
            let mut can_use_this_colour = true;
            'test_this_colour: for (i, edge) in g.adj[next_vert].iter_enum() {
                if *edge && colour[i] == Some(col) { // can't colour that
                    can_use_this_colour = false;
                    break 'test_this_colour;
                }
            }
            if can_use_this_colour {
                colour[next_vert] = Some(col);
                if can_be_coloured_rec(g, num_colours, usize::max(max_colour_used, col), colour, next_vert.incr()) {
                    can_colour = true;
                    break 'test_all_colourings;
                } else {
                    colour[next_vert] = None;
                }
            }
        }
        can_colour
    }
}

fn can_be_coloured(g: &Graph, num_colours: usize) -> bool {
    let mut colour = VertexVec::new(g.n, &None);
    colour[Vertex::ZERO] = Some(0);
    can_be_coloured_rec(g, num_colours, 0, &mut colour, Vertex::ZERO.incr())
}

pub fn chromatic_number(g: &Graph) -> u32 {
    let mut num_colours = 1;
    'find_chromatic_number: loop {
        if can_be_coloured(g, num_colours) {
            break 'find_chromatic_number;
        } else {
            num_colours += 1;
        }
    }
    num_colours as u32
}

fn alice_wins_chromatic_game_rec(g: &Graph, k: usize, max_colour_used: usize, 
        colour: &mut VertexVec<Option<usize>>, num_cold: usize) -> bool {
    if g.n.at_most(num_cold) {
        // G is coloured, so Alice has won
        true
    } else {
        let mut alice_win = num_cold % 2 == 1;
        let mut is_something_playable = false;
        let col_cap = (max_colour_used + 2).min(k);

        'test_verts: for v in g.iter_verts() {
            if colour[v].is_none() {
                for c in 0..col_cap {
                    let mut can_use_c = true;
                    'test_c: for u in g.adj_list[v].iter() {
                        if colour[*u] == Some(c) {
                            can_use_c = false;
                            break 'test_c;
                        }
                    }
                    if can_use_c {
                        is_something_playable = true;
                        colour[v] = Some(c);
                        let max_col = max_colour_used.max(c);
                        let sub_alice_win = alice_wins_chromatic_game_rec(g, k, max_col, colour, num_cold + 1);
                        if sub_alice_win != alice_win {
                            // This is a winning strategy for this player.
                            alice_win = sub_alice_win;
                            colour[v] = None;
                            break 'test_verts;
                        }
                        colour[v] = None;
                    }
                }
            }
        }
        if is_something_playable {
            alice_win
        } else {
            // Nothing can be played, so Bob wins.
            false
        }
    }
}

// if (k+1)th smallest degree is <k then Alice can certainly win for k colours.
fn alice_greedy_lower_bound(g: &Graph) -> usize {
    let mut degs = g.degree_sequence().to_owned();
    degs.sort();
    for (k, d) in degs.iter().rev().enumerate() {
        if d.to_usize() < k {
            return k;
        }
    }
    return g.n.to_usize();
}

pub fn alice_wins_chromatic_game(g: &Graph, k: usize) -> bool {
    if k >= alice_greedy_lower_bound(g) {
        return true;
    }
    let mut colour = VertexVec::new(g.n, &None);
    for v in g.iter_verts() {
        colour[v] = Some(0);
        if alice_wins_chromatic_game_rec(g, k, 0, &mut colour, 1) {
            return true;
        }
        colour[v] = None;
    }
    false
}

pub fn game_chromatic_number(g: &Graph) -> u32 {
    let mut k = 1;
    loop {
        if alice_wins_chromatic_game(g, k) {
            return k as u32;
        }
        k += 1;
    }
}

pub fn print_game_chromatic_table(g: &Graph) {
    let delta = g.max_degree();
    for k in 1..=(delta + 1) {
        if alice_wins_chromatic_game(g, k as usize) {
            println!("{}: Alice", k);
        } else {
            println!("{}: Bob", k);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::graph::*;
    use super::*;
    use utilities::*;

    #[test]
    fn test_chi_1() {
        assert_eq!(chromatic_number(&Graph::test_graph(1)), 4);
    }

    #[test]
    fn test_chi_2() {
        assert_eq!(chromatic_number(&Graph::test_graph(2)), 3);
    }

    #[test]
    fn test_chi_3() {
        assert_eq!(chromatic_number(&Graph::test_graph(3)), 2);
    }

    #[test]
    fn test_chi_e10() {
        assert_eq!(chromatic_number(&Graph::new_empty(Order::of_usize(10))), 1);
    }
}