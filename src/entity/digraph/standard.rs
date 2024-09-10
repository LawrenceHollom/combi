use super::Digraph;

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
}