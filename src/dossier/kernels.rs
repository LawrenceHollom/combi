use crate::entity::digraph::*;

use utilities::vertex_tools::*;

fn min_kernel_internal(d: &Digraph, stop_threshold: usize) -> VertexSet {
    let out_nbrs = d.get_out_nbhds();
    let in_nbrs = d.get_in_nbhds();
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
    let mut s = VertexSet::of_vert(d.n, Vertex::ZERO);

    'iter_subsets: while s.is_in_range() {
        for v in s.iter_rev() {
            let inter = out_nbrs[v].inter(&s);
            if inter.is_nonempty() {
                // s isn't a stable set.
                let w = inter.get_biggest_element().unwrap();
                // v and w can't both be present, so we can increment s to avoid this conflict.
                s.incr_inplace_to_remove_vertex(v.min(w));
                continue 'iter_subsets;
            }
        }

        let size = s.size();
        if size >= min_kernel_size {
            // Don't bother with big things.
            s.incr_inplace();
            continue 'iter_subsets;
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
            if size <= stop_threshold {
                // We've found something small *enough*, so pass it back.
                return optimal_set;
            }
        }
        s.incr_inplace();
    }

    optimal_set
}

pub fn min_kernel_size(d: &Digraph, print_min: bool) -> u32 {
    let optimal_set = min_kernel_internal(d, 0);

    if print_min {
        print!("Minimal 2-kernel: ");
        optimal_set.print_hum();
    }

    optimal_set.size() as u32
}

pub fn has_kernel_of_size_at_most(d: &Digraph, size: usize) -> bool {
    let optimal_set = min_kernel_internal(d, size);
    optimal_set.size() <= size
}
