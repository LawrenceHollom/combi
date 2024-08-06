use crate::entity::graph::*;

pub fn is_triangle_free(g: &Graph) -> bool {
    let mut is_triangle_free = true;
    
    'test_verts: for u in g.n.iter_verts() {
        for (i, v) in g.adj_list[u].iter().enumerate() {
            for (j, w) in g.adj_list[u].iter().enumerate() {
                if j > i && g.adj[*v][*w] {
                    is_triangle_free = false;
                    break 'test_verts;
                }
            }
        }
    }

    is_triangle_free
}