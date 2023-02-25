use crate::graph::*;

pub fn is_triangle_free(g: &Graph) -> bool {
    let mut is_triangle_free = true;
    let n = g.n.to_usize();
    
    'test_verts: for u in 0..n {
        for (i, v) in g.adj_list[u].iter().enumerate() {
            for (j, w) in g.adj_list[u].iter().enumerate() {
                if j > i {
                    if g.adj[*v][*w] {
                        is_triangle_free = false;
                        break 'test_verts;
                    }
                }
            }
        }
    }

    is_triangle_free
}