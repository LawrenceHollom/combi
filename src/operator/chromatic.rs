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