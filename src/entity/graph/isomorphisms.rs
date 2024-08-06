use utilities::vertex_tools::*;
use crate::entity::graph::*;

fn codeg_code(u: Vertex, v: Vertex) -> usize {
    Edge::of_pair(u, v).encode()
}

fn codegree_sequence(g: &Graph) -> Vec<usize> {
    let n = g.n.to_usize();
    let mut codegs = vec![0; (n * (n - 1)) / 2];

    for nbrs in g.adj_list.iter() {
        for u in nbrs.iter() {
            for v in nbrs.iter() {
                if *u < *v {
                    codegs[codeg_code(*u, *v)] += 1;
                }
            }
        }
    }

    codegs
}

fn is_map_isomorphism(h: &Graph, g: &Graph, map: &VertexVec<Option<Vertex>>) -> bool {
    for (i, j) in h.n.iter_pairs() {
        match (map[i], map[j]) {
            (Some(x), Some(y)) => {
                if h.adj[i][j] != g.adj[x][y] {
                    return false;
                }
            }
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => return false,

        }
    }
    true
}

fn is_isomorphic_to_rec(h: &Graph, g: &Graph, ordering: &VertexVec<Vertex>, map: &mut VertexVec<Option<Vertex>>, 
        covered: &mut VertexVec<bool>, node: Vertex, self_codegs: &Vec<usize>, g_codegs: &Vec<usize>) -> bool {
    if node.is_n(h.n) {
        is_map_isomorphism(h, g, map)
    } else {
        let mut is_any_iso = false;
        let v = ordering[node];
        // i is the target location of node
        'find_iso: for i in h.n.iter_verts() {
            if h.deg[v] == g.deg[i] && !covered[i] {
                let mut adj_check = true;
                'adj_test: for u in h.adj_list[v].iter() {
                    if map[*u].map_or(false, |x| !g.adj[x][i]
                            || self_codegs[codeg_code(*u, v)] != g_codegs[codeg_code(x, i)]) {
                        adj_check = false;
                        break 'adj_test;
                    }
                }
                if adj_check {
                    map[v] = Some(i);
                    covered[i] = true;
                    if is_isomorphic_to_rec(h, g, ordering, map, covered, node.incr(), self_codegs, g_codegs) {
                        is_any_iso = true;
                        break 'find_iso;
                    }
                    map[v] = None;
                    covered[i] = false;
                }
            }
        }
        is_any_iso
    }
}

pub fn is_isomorphic_to(h: &Graph, g: &Graph) -> bool {
    let mut self_degs = h.deg.to_owned();
    let mut g_degs = g.deg.to_owned();
    self_degs.sort(Degree::cmp);
    g_degs.sort(Degree::cmp);

    if h.n != g.n {
        return false;
    }
    let n = h.n.to_usize();

    if !self_degs.iter().zip(g_degs.iter()).all(|(x, y)| *x == *y) {
        return false;
    }

    let mut self_codegs = codegree_sequence(h);
    let mut g_codegs = codegree_sequence(g);
    self_codegs.sort();
    g_codegs.sort();

    if !self_codegs.iter().zip(g_codegs.iter()).all(|(x, y)| *x == *y) {
        //println!("Gottem! {} ~ {}", h.constructor, g.constructor);
        return false;
    }

    let mut self_comps = h.component_sizes();
    let mut g_comps = g.component_sizes();
    self_comps.sort();
    g_comps.sort();

    if !self_comps.iter().zip(g_comps.iter()).all(|(x, y)| *x == *y) {
        //println!("Connectedness catch!");
        return false;
    }

    if n > 15 { 
        // give up; would be too slow
        return false;
    }

    // BFS on self to find ordering.
    let mut q: Queue<Vertex> = queue![];
    let mut ordering = VertexVec::new(h.n, &Vertex::ZERO);
    let mut visited = VertexVec::new(h.n, &false);
    let mut next_preimage = Vertex::ZERO;
    for start in h.n.iter_verts() {
        if !visited[start] {
            visited[start] = true;
            let _ = q.add(start);
        }
        'bfs: loop {
            match q.remove() {
                Ok(node) => {
                    ordering[next_preimage] = node;
                    next_preimage.incr_inplace();
                    for v in h.adj_list[node].iter() {
                        if !visited[*v] {
                            visited[*v] = true;
                            let _ = q.add(*v);
                        }
                    }
                },
                Err(_) => break 'bfs,
            }
        }
    }

    let is_iso = is_isomorphic_to_rec(h, g, &ordering, &mut VertexVec::new(h.n, &None), &mut VertexVec::new(h.n, &false), Vertex::ZERO,
            &codegree_sequence(h), &codegree_sequence(g));
    if !is_iso {
        println!("Missed: {} !~ {}", h.constructor, g.constructor);
    }
    is_iso
}