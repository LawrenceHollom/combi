use crate::entity::graph::*;

use utilities::vertex_tools::*;

// n.b. Same as max_acyclic

fn max_induced_forest_rec(g: &Graph, forest: &mut VertexVec<bool>, comp: &mut VertexVec<Vertex>, 
        prev_vert: Vertex, size: u32, max_size: u32) -> u32 {
    let max_vert = Vertex::of_usize((size + (g.n.to_usize() as u32) - max_size - 1) as usize).min(g.n.to_max_vertex());
    if max_vert < prev_vert {
        return 0;
    }
    let mut best_size = size;
    for v in prev_vert.incr().iter_from_to_incl(max_vert) {
        // Try adding vertex v. Only bad if it creates a cycle.
        let mut comp_found = VertexVec::new(g.n, &false);
        let mut is_vertex_good = true;
        'test_nbrs: for w in g.adj_list[v].iter() {
            let mut x = comp[*w];
            while comp[x] != x {
                x = comp[x];
            }
            if comp_found[x] {
                is_vertex_good = false;
                break 'test_nbrs;
            } else {
                comp_found[x] = true;
            }
        }

        if is_vertex_good {
            // adjust the union-find; it doesn't make a cycle
            let mut new_comp = comp.to_owned(); // cri.
            for w in g.adj_list[v].iter() {
                if forest[*w] {
                    let mut x = new_comp[*w];
                    while new_comp[x] != x {
                        let sta = new_comp[x];
                        new_comp[x] = v;
                        x = sta;
                    }
                    new_comp[x] = v;
                }
            }
            forest[v] = true;
            let this_size = max_induced_forest_rec(g, forest, &mut new_comp, v, size + 1, max_size);
            if this_size > best_size {
                best_size = this_size;
            }
            forest[v] = false;
        }
    }
    best_size
}

pub fn max_induced_forest(g: &Graph) -> u32 {
    let mut size = 0;
    let mut forest = VertexVec::new(g.n, &false);
    let mut comp = VertexVec::new_fn(g.n, |v| v);

    'test_start_vertex: for v in g.n.iter_verts() {
        if v.num_verts_after(g.n) <= size as usize {
            break 'test_start_vertex;
        }
        forest[v] = true;
        let this_size = max_induced_forest_rec(g, &mut forest, &mut comp, v, 1, size);
        if this_size > size {
            size = this_size;
        }
        forest[v] = false;
    }
    size
}

#[cfg(test)]
mod tests {
    use super::*;
    use utilities::*;

    #[test]
    fn test_max_induced_forest_1() {
        assert_eq!(max_induced_forest(&Graph::test_graph(1)), 5);
    }

    #[test]
    fn test_max_induced_forest_2() {
        assert_eq!(max_induced_forest(&Graph::test_graph(2)), 10);
    }

    #[test]
    fn test_max_induced_forest_3() {
        assert_eq!(max_induced_forest(&Graph::test_graph(3)), 7);
    }

    #[test]
    fn test_max_induced_forest_k10() {
        assert_eq!(max_induced_forest(&Graph::new_complete(Order::of_usize(10))), 2);
    }

    #[test]
    fn test_max_induced_forest_p5() {
        assert_eq!(max_induced_forest(&Graph::new_path(Order::of_usize(5))), 5);
    }
}