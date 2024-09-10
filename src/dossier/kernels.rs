use crate::entity::digraph::*;

use utilities::vertex_tools::*;

pub fn min_kernel_size(d: &Digraph, print_min: bool) -> u32 {
    let out_nbrs = d.get_out_nbhds();
    let in_nbrs = d.get_out_nbhds();
    let mut covers = VertexVec::new(d.n, &VertexSet::new(d.n));
    for x in d.iter_verts() {
        covers[x].add_vert(x);
        covers[x].add_all(out_nbrs[x]);
        for y in d.iter_verts() {
            if out_nbrs[x].inter(&in_nbrs[y]).is_nonempty() {
                covers[x].add_vert(y);
            }
        }
    }

    let mut min_kernel_size = usize::MAX;
    let mut optimal_set = VertexSet::new(d.n);

    'iter_subsets: for s in d.iter_vertex_subsets() {
        let size = s.size();
        if size >= min_kernel_size {
            // Don't bother with big things.
            continue 'iter_subsets
        }
        for v in s.iter() {
            if out_nbrs[v].inter(&s).is_nonempty() {
                // s isn't a stable set.
                continue 'iter_subsets
            }
        }

        // This is now a candidate optimal kernel, but is it a kernel?

        let mut covered_verts = VertexSet::new(d.n);
        for v in s.iter() {
            covered_verts.add_all(covers[v])
        }

        if covered_verts.is_everything() {
            // This is a kernel.
            min_kernel_size = size;
            optimal_set = s;
        }
    }

    if print_min {
        println!("Optimal set: ");
        optimal_set.print_hum();
    }

    min_kernel_size as u32
}