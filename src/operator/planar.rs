use crate::graph::*;

use utilities::vertex_tools::*;
use utilities::edge_tools::*;
use utilities::component_tools::*;

use std::collections::HashSet;

fn dmp_embed_edge(g: &Graph, coprime: &mut VertexVec<bool>, prime_edges: &mut VertexVec<VertexVec<bool>>,
        faces: &mut HashSet<Vec<Vertex>>, admissible_face: &Vec<Vertex>, e: Edge, depth: u32) -> bool {
    let i = e.fst();
    let j = e.snd();
    prime_edges[i][j] = true;
    prime_edges[j][i] = true;
    if faces.len() > 1 {
        // A hack to deal with how the very first face should appear twice.
        faces.remove(admissible_face);
    }
    let mut i_index = 0;
    while admissible_face[i_index] != i {
        i_index += 1;
    }
    let mut j_index = 0;
    while admissible_face[j_index] != j {
        j_index += 1;
    }
    if i_index > j_index {
        (i_index, j_index) = (j_index, i_index);
    }
    let mut face1 = vec![];
    for u in admissible_face.iter().take(i_index + 1) {
        face1.push(*u);
    }
    for u in admissible_face.iter().skip(j_index) {
        face1.push(*u);
    }
    let mut face2 = vec![];
    for u in admissible_face.iter().take(j_index + 1).skip(i_index) {
        face2.push(*u);
    }
    faces.insert(face1);
    faces.insert(face2);
    dmp_rec(g, coprime, prime_edges, faces, depth + 1)
}

fn dmp_embed_fragment(g: &Graph, coprime: &mut VertexVec<bool>, prime_edges: &mut VertexVec<VertexVec<bool>>,
        faces: &mut HashSet<Vec<Vertex>>, admissible_face: &Vec<Vertex>,
        contact_edge: Edge, depth: u32) -> bool {
    // Flood fill to find a path through the fragment from u to v.
    let contact_u = contact_edge.fst();
    let contact_v = contact_edge.snd();
    let prev = g.flood_fill(contact_u, Some(contact_v), Some(coprime));

    let mut alpha_path = vec![contact_v];
    let mut node = contact_v;
    while node != contact_u {
        node = prev[node].unwrap();
        alpha_path.push(node);
    }

    // Now we have alpha path. So we need to split faces and add prime edges.
    for v in alpha_path.iter() {
        coprime[*v] = false;
    }
    for i in 0..alpha_path.len() - 1 {
        let u = alpha_path[i];
        let v = alpha_path[i + 1];
        prime_edges[u][v] = true;
        prime_edges[v][u] = true;
    }
    if faces.len() > 1 {
        // A hack to deal with how the very first face should appear twice.
        faces.remove(admissible_face);
    }

    // Add in the new faces.
    let mut u_index = 0;
    while admissible_face[u_index] != contact_u {
        u_index += 1;
    }
    let mut v_index = 0;
    while admissible_face[v_index] != contact_v {
        v_index += 1;
    }

    let mut face1: Vec<Vertex> = vec![];
    let mut face2: Vec<Vertex> = vec![];
    let min = u_index.min(v_index);
    let max = u_index.max(v_index);
    for u in admissible_face.iter().take(min) {
        face1.push(*u);
    }
    for u in admissible_face.iter().take(max).skip(min + 1) {
        face2.push(*u);
    }
    
    // This could surely be rewritten to avoid the code repetition.
    if v_index < u_index {
        for x in alpha_path.iter() {
            face1.push(*x);
        }
        for x in alpha_path.iter().rev() {
            face2.push(*x);
        }
    } else {
        for x in alpha_path.iter().rev() {
            face1.push(*x);
        }
        for x in alpha_path.iter() {
            face2.push(*x);
        }
    }
    for u in admissible_face.iter().skip(max + 1) {
        face1.push(*u);
    }

    faces.insert(face1);
    faces.insert(face2);

    dmp_rec(g, coprime, prime_edges, faces, depth + 1)
}

// Actually run the DMP algorithm recursively
fn dmp_rec(g: &Graph, coprime: &mut VertexVec<bool>, prime_edges: &mut VertexVec<VertexVec<bool>>, 
        faces: &mut HashSet<Vec<Vertex>>, depth: u32) -> bool {
    if depth > 10000 {
        println!("TOO DEEP!");
        g.print_matrix();
        println!("Prime edges: {:?}", prime_edges);
        println!("Faces: {:?}", faces);
        println!("Coprime: {:?}", coprime);
        panic!("Too deep!");
    }
    let n = g.n.to_usize();

    let mut is_everything = true;
    'test_if_everything: for (i, i_prime_edges) in prime_edges.iter_enum() {
        for (j, ij_prime_edges) in i_prime_edges.iter_enum() {
            if g.adj[i][j] != *ij_prime_edges {
                is_everything = false;
                break 'test_if_everything;
            }
        }
    }

    if is_everything {
        // We have found an embedding!
        return true;
    }

    // Compute the set of fragments
    let mut edge_fragments: Vec<Edge> = vec![];
    // Just do it slowly for now.
    for i in g.n.iter_verts() {
        if !coprime[i] {
            for j in g.adj_list[i].iter() {
                if !coprime[*j] && !prime_edges[i][*j] {
                    edge_fragments.push(Edge::of_pair(i, *j));
                }
            }
        }
    }

    let frag_comps = g.filtered_components(Some(coprime));
    let mut frag_indices: Vec<Component> = vec![];
    let mut found_comp = ComponentVec::new(g.n, &false);
    for i in g.n.iter_verts() {
        if coprime[i] && !found_comp[frag_comps[i]] {
            frag_indices.push(frag_comps[i]);
            found_comp[frag_comps[i]] = true;
        }
    }
    // So the fragments are edge_fragments and frag_indices.

    // Edge-fragments
    let num_edge_frags = edge_fragments.len();
    let mut first_edge_admissible_face: &Vec<Vertex> = &vec![];
    let mut admissible_face: &Vec<Vertex> = &vec![];

    for (edge_index, e) in edge_fragments.iter().enumerate() {
        let mut num_found = 0;
        'iter_faces: for face in faces.iter() {
            if face.contains(&e.fst()) && face.contains(&e.snd()) {
                // It is admissible.
                num_found += 1;
                if num_found == 1 {
                    admissible_face = face;
                    if edge_index == 0 {
                        first_edge_admissible_face = face;
                    }
                } else {
                    break 'iter_faces;
                }
            }
        }
        if num_found == 0 {
            // Has no admissible faces, so not planar.
            return false;
        }
        if num_found == 1 {
            // We can embed this edge and move on.
            return dmp_embed_edge(g, coprime, prime_edges, &mut faces.to_owned(), 
                admissible_face, *e, depth);
        }
    }

    let mut first_frag_admissible_face: &Vec<Vertex> = &vec![];
    let mut first_frag_contact_edge = None;
    let mut fragment: Vec<Vertex> = vec![];

    // Big fragments
    for (frag_index, frag) in frag_indices.iter().enumerate() {
        let mut contact_verts: Vec<Vertex> = vec![];
        let mut found = VertexVec::new(g.n, &false);
        for i in g.n.iter_verts() {
            if frag_comps[i] == *frag {
                fragment.push(i);
                for j in g.adj_list[i].iter() {
                    if !coprime[*j] && !found[*j] {
                        contact_verts.push(*j);
                        found[*j] = true;
                    }
                }
            }
        }

        let mut num_found = 0;
        // We have the contact vertices. Find the admissible faces.
        'test_faces: for face in faces.iter() {
            if contact_verts.iter().all(|x| face.contains(x)) {
                num_found += 1;
                if num_found == 1 {
                    admissible_face = face;
                    if frag_index == 0 {
                        first_frag_admissible_face = face;
                        first_frag_contact_edge = Some(Edge::of_pair(contact_verts[0], contact_verts[1]));
                    }
                } else if num_found == 2 {
                    break 'test_faces;
                }
            }
        }

        if num_found == 0 {
            return false;
        }
        if num_found == 1 {
            // We need to embed a path from this fragment.
            return dmp_embed_fragment(g, coprime, prime_edges, &mut faces.to_owned(), 
                admissible_face, Edge::of_pair(contact_verts[0], contact_verts[1]), depth);
        }
    }
    
    // We still haven't embedded anything, so we need to just embed something and recurse
    if num_edge_frags > 0 {
        dmp_embed_edge(g, coprime, prime_edges, &mut faces.to_owned(), 
            first_edge_admissible_face, edge_fragments[0], depth)
    } else {
        dmp_embed_fragment(g, coprime, prime_edges, &mut faces.to_owned(), 
            first_frag_admissible_face, first_frag_contact_edge.unwrap(), depth)
    }
}

// We may now assume that g is 2-connected and has at least 5 vertices.
fn is_two_connected_planar(g: &Graph) -> bool {
    // Find a cycle of G.
    let mut cocycle = VertexVec::new(g.n, &true);
    let mut face = vec![];
    let mut cycle_edges: VertexVec<VertexVec<bool>> = VertexVec::new(g.n, &VertexVec::new(g.n, &false));
    let mut head = Vertex::ZERO;
    let mut prev = None;
    // This probably has to succeed due to 2-connectedness.
    while cocycle[head] {
        cocycle[head] = false;
        face.push(head);
        match prev {
            Some(prev) => {
                cycle_edges[prev][head] = true;
                cycle_edges[head][prev] = true;
            }
            None => ()
        }
        'find_next: for v in g.adj_list[head].iter() {
            if prev != Some(*v) {
                prev = Some(head);
                head = *v;
                break 'find_next;
            }
        }
    }

    cycle_edges[prev.unwrap()][head] = true;
    cycle_edges[head][prev.unwrap()] = true;

    let mut faces = HashSet::new();
    faces.insert(face);

    dmp_rec(g, &mut cocycle, &mut cycle_edges, &mut faces, 0)
}

// We may assume that g is connected. Split into 2-connected components.
fn is_component_planar_rec(g: &Graph, filter: &mut VertexVec<bool>) -> bool {
    // First trim all leaves.
    let mut leaf_trimmed = true;
    while leaf_trimmed {
        leaf_trimmed = false;
        for i in g.n.iter_verts() {
            if filter[i] && g.filtered_degree(i, filter) <= 1 {
                filter[i] = false;
                leaf_trimmed = true;
            }
        }
    }

    // Check it's not obviously planar
    let verts= filter.iter().filter(|x| **x).count();
    let edges = g.filtered_size(filter);
    if verts <= 4 { 
        true
    } else if edges > 3 * verts - 6 {
        false
    } else {
        // split G into 2-connected components and run on them seperately.
        let mut cutvertex = None;
        'find_cutvertex: for i in g.n.iter_verts() {
            if filter[i] {
                filter[i] = false;
                if g.num_filtered_components(Some(filter)) > 1 {
                    cutvertex = Some(i);
                    break 'find_cutvertex;
                }
                filter[i] = true;
            }
        }
        match cutvertex {
            None => is_two_connected_planar(&g.of_filtered(filter)),
            Some(cutvertex) => {
                // filter[cutvertex] still false from above.
                let comps = g.filtered_components(Some(filter));
                let mut is_comp = ComponentVec::new(g.n, &false);
                let mut is_planar = true;
                'component_checks: for i in g.n.iter_verts() {
                    if filter[i] && !is_comp[comps[i]] {
                        let mut comp_filter = VertexVec::new(g.n, &false);
                        is_comp[comps[i]] = true;
                        for j in g.n.iter_verts() {
                            if filter[j] && comps[j] == comps[i] {
                                comp_filter[j] = true;
                            }
                        }
                        comp_filter[cutvertex] = true;
                        if !is_component_planar_rec(g, &mut comp_filter) {
                            is_planar = false;
                            break 'component_checks;
                        }
                    }
                }
                is_planar
            }
        }
        
    }
}

pub fn is_planar(g: &Graph) -> bool {
    let comps = g.components();
    let n = g.n.to_usize();
    let mut comp_processed = ComponentVec::new(g.n, &false);
    let mut is_planar = true;
    'component_search: for i in g.n.iter_verts() {
        if !comp_processed[comps[i]] {
            comp_processed[comps[i]] = true;
            let mut filter: VertexVec<bool> = comps.iter().map(|x| *x == comps[i]).collect();
            if !is_component_planar_rec(g, &mut filter) {
                is_planar = false;
                break 'component_search;
            }
        }
    }
    is_planar
}