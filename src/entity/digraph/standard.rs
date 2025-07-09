use super::Digraph;

use utilities::vertex_tools::*;

impl Digraph {
    pub fn has_source(&self) -> bool {
        self.iter_verts().any(|v| self.in_deg[v].equals(0))
    }

    pub fn has_sink(&self) -> bool {
        self.iter_verts().any(|v| self.out_deg[v].equals(0))
    }

    /**
     * Returns v, where v[x] is the set of all those vertices y which are
     * connected by an edge x -> y.
     */
    pub fn get_out_nbhds(&self) -> VertexVec<VertexSet> {
        let mut out = VertexVec::new(self.n, &VertexSet::new(self.n));

        for x in self.iter_verts() {
            for y in self.out_adj_list[x].iter() {
                out[x].add_vert(*y);
            }
        }

        out
    }

    /**
     * Returns v, where v[x] is the set of all those vertices y which are
     * connected by an edge x <- y.
     */
    pub fn get_in_nbhds(&self) -> VertexVec<VertexSet> {
        let mut out = VertexVec::new(self.n, &VertexSet::new(self.n));

        for x in self.iter_verts() {
            for y in self.in_adj_list[x].iter() {
                out[x].add_vert(*y);
            }
        }

        out
    }

    /**
     * Returns a matrix m where m[x][y] is the min distance from x to y, or None if there
     * is no path from x to y.
     * Runs in n^3 time, small constant.
     * dist[x][x] is only returned as Some if there is a nonempty path from x to x; in particular,
     * x is not a sink.
     */
    pub fn floyd_warshall(&self) -> VertexVec<VertexVec<Option<u32>>> {
        let mut out = VertexVec::new(self.n, &VertexVec::new(self.n, &None));

        for (u, v) in self.iter_pairs() {
            if self.adj[u][v] {
                out[u][v] = Some(1);
            }
            if self.adj[v][u] {
                out[v][u] = Some(1);
            }
        }

        // The magic iteration.
        for k in self.iter_verts() {
            for i in self.iter_verts() {
                if let Some(ik_dist) = out[i][k] {
                    for j in self.iter_verts() {
                        if let Some(kj_dist) = out[k][j] {
                            if let Some(ij_dist) = out[i][j] {
                                out[i][j] = Some(ij_dist.min(ik_dist + kj_dist))
                            } else {
                                out[i][j] = Some(ik_dist + kj_dist)
                            }
                        }
                    }
                }
            }
        }

        out
    }
}
