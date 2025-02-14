use utilities::vertex_tools::*;
use crate::entity::graph::*;

impl Graph{
    fn list_cycles_rec(&self, dist: &VertexVec<VertexVec<usize>>, visited: &mut VertexVec<bool>, cycle: &mut Vec<Edge>, 
            current_len: usize, target_len: usize, this_vert: Vertex, start_vert: Vertex, list: &mut Vec<Vec<Edge>>) {
        if current_len == target_len - 1 {
            if self.adj[this_vert][start_vert] {
                cycle.push(Edge::of_pair(this_vert, start_vert));
                list.push(cycle.to_owned());
                let _ = cycle.pop();
            }
        } else {
            // Try adding more vertices.
            for v in self.adj_list[this_vert].iter() {
                if !visited[*v] && dist[start_vert][*v] < target_len - current_len {
                    visited[*v] = true;
                    cycle.push(Edge::of_pair(this_vert, *v));
                    self.list_cycles_rec(dist, visited, cycle, current_len + 1, target_len, *v, start_vert, list);
                    let _ = cycle.pop();
                    visited[*v] = false;
                }
            }
        }
    }

    pub fn is_filtered_acyclic(&self, filter: EdgeSet, indexer: &EdgeIndexer) -> bool {
        // Just bfs, and if we hit the same vertex twice then that's an L.
        let mut is_acyclic = true;
        let mut q: Queue<Vertex> = queue![];
        let mut parent = VertexVec::new(self.n, &None);
        'test_verts: for v in self.iter_verts() {
            if parent[v].is_none() {
                parent[v] = Some(v);
                let _ = q.add(v);
                'bfs: loop {
                    match q.remove() {
                        Ok(u) => {
                            for w in self.adj_list[u].iter() {
                                if *w != parent[u].unwrap() {
                                    // If it is, then we've already sorted it.
                                    let e = Edge::of_pair(*w, u);
                                    if filter.has_edge(e, indexer) {
                                        // We only care if it's in the filter.
                                        if parent[*w].is_some() {
                                            is_acyclic = false;
                                            break 'test_verts;
                                        } else {
                                            parent[*w] = Some(u);
                                            let _ = q.add(*w);
                                        }
                                    }
                                }
                            }
                        }
                        Err(_) => break 'bfs,
                    }
                }
            }
        }
        is_acyclic
    }

    pub fn is_forest(&self) -> bool {
        let indexer = EdgeIndexer::new(&self.adj_list);
        let filter = EdgeSet::new(&indexer).inverse(&indexer);
        self.is_filtered_acyclic(filter, &indexer)
    }

    pub fn remove_all_k_cycles(&mut self, k: usize) {    
        let dist = self.floyd_warshall();

        // DFS to find all cycles.
        let mut list: Vec<Vec<Edge>> = vec![];
        for u in self.n.iter_verts() {
            let mut visited = VertexVec::new(self.n, &false);
            visited[u] = true;
            self.list_cycles_rec(&dist, &mut visited, &mut vec![], 0, k, u, u, &mut list);
        }
        // Now remove edges to kill the cycles.
        let mut removed = EdgeVec::new(&self.adj_list, false);
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
                    let ds = self.deg[e.fst()] + self.deg[e.snd()];
                    if ds > max_sum {
                        max_sum = ds;
                        max_edge = *e;
                    }
                }
                // Remove max_edge
                removed.set(max_edge, true);
                self.delete_edge(max_edge);

            }
        }
    }

    pub fn contains_four_cycle(&self) -> bool {
        let nbhds = self.get_open_nbhds();
        for (u, v) in self.iter_pairs() {
            if nbhds[u].inter(&nbhds[v]).size() >= 2 {
                return true
            }
        }
        false
    }
}