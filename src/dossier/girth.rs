use crate::entity::{*, graph::*, poset::*};
use utilities::vertex_tools::*;

fn graph_girth(g: &Graph) -> u32 {
    let dist = g.floyd_warshall();
    let mut girth = g.n.to_usize();

    for (u, d) in dist.iter_enum() {
        for v in u.incr().iter_from(g.n) {
            // If they're far away there's no point trying
            if d[v] < (girth + 1) / 2 {
                let mut num_less = 0;
                let mut num_equal = 0;
                for w in g.adj_list[v].iter() {
                    use std::cmp::Ordering::*;
                    match d[*w].cmp(&d[v]) {
                        Less => num_less += 1,
                        Equal => num_equal += 1,
                        Greater => ()
                    }
                }
                if num_less >= 2 {
                    girth = 2 * d[v];
                } else if num_equal >= 1 {
                    girth = 2 * d[v] + 1;
                }
            }
        }
    }

    girth as u32
}

/**
 * The girth of a poset is defined (by us) to be the max number of elements
 * incomparable to a single x in P.
 */
fn poset_girth(p: &Poset) -> u32 {
    let mut incomp_sizes = VertexVec::new(p.order, &0);
    for (u, v) in p.iter_pairs() {
        if p.incomparable(u, v) {
            incomp_sizes[u] += 1;
            incomp_sizes[v] += 1;
        }
    }

    let mut girth = 0;
    for v in p.iter_verts() {
        girth = girth.max(incomp_sizes[v])
    }
    girth
}

pub fn girth(e: &Entity) -> u32 {
    use Entity::*;
    match e {
        Graph(g) => graph_girth(g),
        Poset(p) => poset_girth(p),
        Digraph(_d) => todo!("If you want to use digraph girth, you need to code it first."),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use utilities::*;

    #[test]
    fn test_girth_1() {
        assert_eq!(graph_girth(&Graph::test_graph(1)), 3);
    }

    #[test]
    fn test_girth_2() {
        assert_eq!(graph_girth(&Graph::test_graph(2)), 4);
    }

    #[test]
    fn test_girth_3() {
        assert_eq!(graph_girth(&Graph::test_graph(3)), 4);
    }

    #[test]
    fn test_girth_c10() {
        assert_eq!(graph_girth(&Graph::new_cyclic(Order::of_usize(10))), 10);
    }
}