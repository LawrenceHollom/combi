use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use crate::entity::graph::*;

use queues::*;
use rand::{Rng, thread_rng};
use rand::rngs::ThreadRng;
use utilities::*;
use utilities::vertex_tools::*;
use utilities::component_tools::*;

/**
 * This stores a graph along with a collection of information which
 * aims to make it easier to compute various (otherwise expensive)
 * computations on g.
 * In particular, this aims to capture a bunch of information about 
 * Aut(G) which can be used to skip a bunch of steps in iterating 
 * over G.
 * We use random algorithms and good heuristics to do all these
 * computations imperfectly but quickly.
 */


/**
 * This stores an automorphism of the graph.
 */
#[derive(Clone, Debug)]
struct Automorphism {
    map: VertexVec<Vertex>,
}

#[derive(Clone)]
pub struct Annotations {
    n: Order,
    dists: VertexVec<VertexVec<usize>>,
    vertex_hashes: VertexVec<u64>,
    weak_auto_comps: UnionFind,
    strong_auto_comps: UnionFind,
    history: HashMap<VertexSet, VertexSet>,
}

/**
 * This stores a bunch of information about a vertex which we will use
 * as a heuristic for determining autocs. We want this to be strong.
 * In an ideal world this would be a "perfect hash of the graph from the
 * perspective of v", but the difficulty is in arriving at a canonical
 * ordering of the vertices of G in this perspective, so that the hash
 * hashes in a hashful way.
 * Perhaps a silly manual thing--e.g. hashing the 4-onion--would be a
 * good start here, as we could surely hack out some canonical ordering.
 */
#[derive(Hash)]
struct VertexSignature {
    dist_counts: Vec<usize>,
}

impl Automorphism {
    pub fn randomly_extend_map(g: &Graph, hashes: &VertexVec<u64>, partial: &Vec<(Vertex, Vertex)>, rng: &mut ThreadRng) -> Option<Automorphism> {
        let mut map = VertexVec::new(g.n, &None);
        let mut map_inv = VertexVec::new(g.n, &None);
        let mut q = queue![];
        let mut visited = VertexVec::new(g.n, &false);

        for (from, to) in partial.iter() {
            map[*from] = Some(*to);
            map_inv[*to] = Some(*from);
            visited[*from] = true;
        }
        for (from, _to) in partial.iter() {
            for v in g.adj_list[*from].iter() {
                if !visited[*v] {
                    visited[*v] = true;
                    let _ = q.add(v);
                }
            }
        }

        let mut is_autoj_good = true;
        let mut num_placed = 1;
        let mut adj_sets = VertexVec::new(g.n, &VertexSet::new(g.n));
        for (v, adj_set) in adj_sets.iter_mut_enum() {
            for u in g.adj_list[v].iter() {
                adj_set.add_vert(*u);
            }
        }
        'place_vertices: loop {
            match q.remove() {
                Ok(to_place) => {
                    let mut locations = VertexSet::everything(g.n);
                    // Could also rule out nbhds of non-adj things.
                    for (obj, img_opn) in map.iter_enum() {
                        if let Some(img) = img_opn {
                            if g.adj[obj][*to_place] {
                                // The images need to be adjacent too.
                                locations = locations.inter(&adj_sets[*img]);
                            } else {
                                locations = locations.inter(&adj_sets[*img].not());
                            }
                        }
                    }
                    let mut valid_locations = vec![];
                    for v in locations.iter() {
                        if hashes[*to_place] == hashes[v] && map_inv[v].is_none() {
                            valid_locations.push(v)
                        }
                    }
                    if valid_locations.is_empty() {
                        is_autoj_good = false;
                        break 'place_vertices;
                    }
                    
                    num_placed += 1;
                    let destination = valid_locations[rng.gen_range(0..valid_locations.len())];
                    map[*to_place] = Some(destination);
                    map_inv[destination] = Some(*to_place);
                    for w in g.adj_list[*to_place].iter() {
                        if !visited[*w] {
                            visited[*w] = true;
                            let _ = q.add(w);
                        }
                    }
                }
                Err(_e) => {
                    if g.n.more_than(num_placed) {
                        is_autoj_good = false;
                    }
                    break 'place_vertices;
                }
            }
        }
        'test_map: for x in map.iter() {
            if x.is_none() {
                is_autoj_good = false;
                break 'test_map;
            }
        }
        
        if is_autoj_good {
            let map: VertexVec<Vertex> = map.iter().map(|x| x.unwrap()).collect();
            // Now need to test if this map is actually an autoj
            // This is maybe unecessary, but not a bottleneck.
            'test_if_autoj: for (u, v) in g.iter_pairs() {
                if g.adj[u][v] != g.adj[map[u]][map[v]] {
                    is_autoj_good = false;
                    break 'test_if_autoj;
                }
            }
            if is_autoj_good {
                Some(Automorphism{ map })
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (Vertex, &Vertex)> {
        self.map.iter_enum()
    }
}

impl Annotations {
    fn get_hash_paritions(hashes: &VertexVec<u64>) -> HashMap<u64, Vec<Vertex>> {
        let mut partns: HashMap<u64, Vec<Vertex>> = HashMap::new();
        for (v, hash) in hashes.iter_enum(){
            match partns.get_mut(hash) {
                Some(partn) => partn.push(v),
                None => {
                    partns.insert(*hash, vec![v]); 
                    ()
                }
            }
        }
        partns
    }

    /**
     * Every strong-auto-comp is also a weak-auto-comp, so this could be used advngsly
     * We seek to `orbits' of vertices under Aut(G), by finding how things move under
     * arbitrary automorphisms. They are weakly isomorphic, and this property is
     * transitive.
     * 
     * This is the more difficult part.
     * Have lots of attempts of: map a to b, and then build the autoj around this.
     * Use random techniques to keep things interesting.
     */
    fn approximate_weak_auto_comps(g: &Graph, hashes: &VertexVec<u64>) -> UnionFind {
        let mut comps = UnionFind::new(g.n);
        let hash_comps = Self::get_hash_paritions(&hashes);
        let mut rng = thread_rng();

        for (_hash, hash_comp) in hash_comps.iter() {
            for (i, v) in hash_comp.iter().enumerate() {
                for w in hash_comp.iter().skip(i + 1) {
                    if let Some(aut) = Automorphism::randomly_extend_map(&g, &hashes, &vec![(*v, *w)], &mut rng) {
                        // We have an autoj. Hoorah! Now add the information it provides.
                        for (from, to) in aut.iter() {
                            if from != *to {
                                comps.merge(from, *to);
                            }
                        }
                    }
                }
            }
        }
        comps
    }

    /**
     * We seek automorphisms which fix all but two vertices, which are flipped.
     * Then those vertices are strongly isomorphic.
     * This is tested by simply checking if two vertices have the same nbhd
     */
    fn strong_auto_comps(g: &Graph) -> UnionFind {
        let mut comps = UnionFind::new(g.n);
        
        fn just_hash(adjs: &Vec<Vertex>) -> u64 {
            let mut hasher = DefaultHasher::new();
            adjs.hash(&mut hasher);
            hasher.finish()
        }

        let hash_nbhds: VertexVec<u64> = g.adj_list.iter().map(just_hash).collect();
        for (u, v) in g.iter_pairs() {
            if comps.get_component(v).is_vertex(v) && hash_nbhds[u] == hash_nbhds[v] {
                let mut are_equal = true;
                'test_equality: for w in g.iter_verts() {
                    if g.adj[v][w] != g.adj[u][w] {
                        are_equal = false;
                        break 'test_equality;
                    }
                }
                if are_equal {
                    comps.merge(u, v)
                }
            }
        }
        comps
    }

    pub fn new(g: &Graph) -> Annotations {
        let dists = g.floyd_warshall();
        let vertex_hashes = VertexSignature::compute_vertex_hashes(&g, &dists);
        let weak_auto_comps = Self::approximate_weak_auto_comps(&g, &vertex_hashes);
        let strong_auto_comps = Self::strong_auto_comps(&g);
        Annotations {
            n: g.n, 
            dists, 
            vertex_hashes, 
            weak_auto_comps, 
            strong_auto_comps,
            history: HashMap::new(),
        }
    }

    /**
     * Returns a VertexSet of representatives of the weak autoj classes.
     */
    pub fn weak_representatives(&self) -> VertexSet {
        self.weak_auto_comps.get_representatives()
    }

    /**
     * Returns a best-guess of a set of representatives of the automorphism classes of the vertices.
     * This is still somewhat ad-hoc, and may miss some equivalences (thus returning multiple vertices
     * from the same equivalence class.)
     */
    pub fn get_representatives(&mut self, g: &Graph, fixed: VertexSet) -> VertexSet {
        match self.history.get(&fixed) {
            Some(representatives) => *representatives,
            None => {
                // We now need to try and find ways in which this can extend to an autoj.
                // Just do something a bit silly for now.
                let mut rng = thread_rng();
                let mut comps = UnionFind::new(self.n);
                let partial = fixed.iter().map(|x| (x, x)).collect::<Vec<(Vertex, Vertex)>>();
                
                // Maybe use strong equivalence and feed in the comps we know about so
                // it can aim for getting something new. Then stop when it can't find
                // any more information.
                let mut allowed_fails_remaining = 20;
                while allowed_fails_remaining > 0 {
                    if let Some(aut) = Automorphism::randomly_extend_map(g, &self.vertex_hashes, &partial, &mut rng) {
                        for (from, to) in aut.iter() {
                            if from != *to {
                                // Some new information has been discovered
                                comps.merge(from, *to);
                            } else {
                                allowed_fails_remaining -= 1;
                            }
                        }
                    }
                }
                let reps = comps.get_representatives();
                self.history.insert(fixed, reps);
                reps.inter(&fixed.not());
                reps
            }
        }
    }
}

impl VertexSignature {
    fn new(g: &Graph, v: Vertex, dists: &VertexVec<VertexVec<usize>>) -> VertexSignature {
        let mut dist_counts = vec![0; g.n.to_usize()];
        for u in g.iter_verts() {
            if dists[u][v] < usize::MAX {
                dist_counts[dists[u][v]] += 1;
            }
        }
        VertexSignature { dist_counts }
    }

    pub fn compute_vertex_hashes(g: &Graph, dists: &VertexVec<VertexVec<usize>>) -> VertexVec<u64> {
        let mut hashes = VertexVec::new(g.n, &0);
        for (v, hash) in hashes.iter_mut_enum() {
            let sig = VertexSignature::new(g, v, dists);
            let mut hasher = DefaultHasher::new();
            sig.hash(&mut hasher);
            *hash = hasher.finish();
        }
        hashes
    }
}

pub fn print_automorphism_info(g: &Graph) {
    let annotated = Annotations::new(g);
    println!("Dists: {:?}", annotated.dists);
    println!("Hashes: {:?}", annotated.vertex_hashes);
    println!("Strong: {:?}", annotated.strong_auto_comps);
    println!("Weak: {:?}", annotated.weak_auto_comps);
    println!("Weak representatives: {:?}", annotated.weak_representatives());
}