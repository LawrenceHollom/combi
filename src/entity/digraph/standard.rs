use super::Digraph;

use utilities::vertex_tools::*;

impl Digraph {
    pub fn has_source(&self) -> bool {
        for v in self.iter_verts() {
            if self.in_deg[v].equals(0) {
                return true
            }
        }
        false
    }

    pub fn has_sink(&self) -> bool {
        for v in self.iter_verts() {
            if self.out_deg[v].equals(0) {
                return true
            }
        }
        false
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
}