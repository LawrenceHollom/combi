use utilities::vertex_tools::*;
use crate::graph::*;

fn list_cycles_rec(g: &Graph, dist: &VertexVec<VertexVec<usize>>, visited: &mut VertexVec<bool>, cycle: &mut Vec<Edge>, 
        current_len: usize, target_len: usize, this_vert: Vertex, start_vert: Vertex, list: &mut Vec<Vec<Edge>>) {
    if current_len == target_len - 1 {
        if g.adj[this_vert][start_vert] {
            cycle.push(Edge::of_pair(this_vert, start_vert));
            list.push(cycle.to_owned());
            let _ = cycle.pop();
        }
    } else {
        // Try adding more vertices.
        for v in g.adj_list[this_vert].iter() {
            if !visited[*v] && dist[start_vert][*v] < target_len - current_len {
                visited[*v] = true;
                cycle.push(Edge::of_pair(this_vert, *v));
                list_cycles_rec(g, dist, visited, cycle, current_len + 1, target_len, *v, start_vert, list);
                let _ = cycle.pop();
                visited[*v] = false;
            }
        }
    }
}

pub fn remove_all_k_cycles(g: &mut Graph, k: usize) {    
    let dist = g.floyd_warshall();

    // DFS to find all cycles.
    let mut list: Vec<Vec<Edge>> = vec![];
    for u in g.n.iter_verts() {
        let mut visited = VertexVec::new(g.n, &false);
        visited[u] = true;
        list_cycles_rec(g, &dist, &mut visited, &mut vec![],
            0, k, u, u, &mut list);
    }
    // Now remove edges to kill the cycles.
    let mut removed = EdgeVec::new(&g.adj_list, false);
    for cycle in list {
        let mut still_there = true;
        'test_cycle: for e in cycle.iter() {
            if removed.get(*e) {
                still_there = false;
                break 'test_cycle;
            }
        }
        if still_there {
            // Remove the edge of largest deg_sum.
            let mut max_sum = Degree::ZERO;
            let mut max_edge = cycle[0];
            for e in cycle.iter() {
                let ds = g.deg[e.fst()] + g.deg[e.snd()];
                if ds > max_sum {
                    max_sum = ds;
                    max_edge = *e;
                }
            }
            // Remove max_edge
            removed.set(max_edge, true);
            g.delete_edge(max_edge);

        }
    }
}