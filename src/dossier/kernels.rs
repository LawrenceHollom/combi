use crate::entity::digraph::*;

use utilities::vertex_tools::*;

pub fn min_kernel_size(d: &Digraph, print_min: bool) -> u32 {
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
                // print!("Not stable!"); s.print_hum();
                let w = inter.get_biggest_element().unwrap();
                // v and w can't both be present, so we can increment s to avoid this conflict.
                s.incr_inplace_to_remove_vertex(v.min(w));
                continue 'iter_subsets
            } else {
                // print!("Stable! ");
            }
        }

        let size = s.size();
        if size >= min_kernel_size {
            // Don't bother with big things.
            // print!("Too big!"); s.print_hum();
            s.incr_inplace();
            continue 'iter_subsets
        }

        // This is now a candidate optimal kernel, but is it a kernel?

        let mut covered_verts = VertexSet::new(d.n);
        for v in s.iter() {
            covered_verts.add_all(covers[v])
        }

        if covered_verts.is_everything() {
            // This is a kernel.
            // print!("Just right!");
            min_kernel_size = size;
            optimal_set = s;
        }
        // s.print_hum();
        s.incr_inplace();
    }

    // if min_kernel_size != new_min_kernel_size {
    //     d.print();
    //     panic!("Old and new calculations disagree!")
    // }

    min_kernel_size as u32
}