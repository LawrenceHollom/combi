mod random_construction;

use queues::*;
use std::usize;

use utilities::*;
use utilities::vertex_tools::*;

use crate::constructor::*;

use crate::dossier::*;
use crate::entity::*;

use super::Graph;

#[derive(Clone)]
pub struct Poset {
    pub order: Order,
    pub gt: VertexVec<VertexVec<bool>>,
    pub covers: VertexVec<VertexSet>,
    pub covered_by: VertexVec<VertexSet>,
    pub downsets: VertexVec<VertexSet>,
    pub upsets: VertexVec<VertexSet>,
    pub heights: VertexVec<usize>,
    pub height: usize,
    constructor: Constructor,
}

/***
 *     covers[v]
 *        |
 *        v
 *        |
 *   covered_by[v]
 */

impl Poset {
    pub fn new_chain(order: Order) -> Poset {
        let gt = VertexVec::new_fn(order, 
            |u| VertexVec::new_fn(order, |v| u > v));
        let mut covers = VertexVec::new(order, &VertexSet::new(order));
        let mut covered_by = VertexVec::new(order, &VertexSet::new(order));
        let mut downsets = VertexVec::new(order, &VertexSet::new(order));
        let mut upsets = VertexVec::new(order, &VertexSet::new(order));
        let mut heights = VertexVec::new(order, &0);
        for (i, v) in order.iter_verts().enumerate() {
            if v != Vertex::ZERO {
                covers[v].add_vert(v.decr());
            }
            if v != order.to_max_vertex() {
                covered_by[v].add_vert(v.incr())
            }
            heights[v] = i;
            for u in order.iter_verts() {
                if u < v {
                    downsets[v].add_vert(u);
                    upsets[u].add_vert(v);
                }
            }
        }
        Poset {
            order,
            gt,
            covers,
            covered_by,
            downsets,
            upsets,
            heights,
            height: order.to_usize(),
            constructor: Constructor::PosetConstr(PosetConstructor::Chain(order))
        }
    }

    pub fn new_antichain(order: Order) -> Poset {
        Poset {
            order,
            gt: VertexVec::new(order, &VertexVec::new(order, &false)),
            covers: VertexVec::new(order, &VertexSet::new(order)),
            covered_by: VertexVec::new(order, &VertexSet::new(order)),
            downsets: VertexVec::new(order, &VertexSet::new(order)),
            upsets: VertexVec::new(order, &VertexSet::new(order)),
            heights: VertexVec::new(order, &0),
            height: 1,
            constructor: Constructor::PosetConstr(PosetConstructor::Antichain(order))
        }
    }

    pub fn get_intersection_with(&self, q: &VertexVec<VertexVec<bool>>) -> VertexVec<VertexVec<bool>> {
        if q.len() != self.order {
            panic!("Cannot intersect posets of different orders!")
        }
        VertexVec::new_fn(self.order, |u| VertexVec::new_fn(self.order, |v| self.gt[u][v] && q[u][v]))
    }

    pub fn of_ordering(gt: VertexVec<VertexVec<bool>>, constructor: Constructor) -> Poset {
        let order = gt.len();
        let mut covers = VertexVec::new(order, &VertexSet::new(order));
        let mut covered_by = VertexVec::new(order, &VertexSet::new(order));

        let mut downsets = VertexVec::new(order, &VertexSet::new(order));
        let mut upsets = VertexVec::new(order, &VertexSet::new(order));

        for (u, v) in order.iter_pairs() {
            if gt[u][v] {
                upsets[v].add_vert(u);
                downsets[u].add_vert(v);
            } else if gt[v][u] {
                downsets[v].add_vert(u);
                upsets[u].add_vert(v);
            }
        }

        for (u, v) in order.iter_pairs() {
            if gt[u][v] && downsets[u].inter(&upsets[v]).is_empty() {
                covered_by[u].add_vert(v);
                covers[v].add_vert(u);
            }
            if gt[v][u] && upsets[u].inter(&downsets[v]).is_empty() {
                covers[u].add_vert(v);
                covered_by[v].add_vert(u);
            }
        }
        
        fn compute_height_rec(order: Order, covered_by: &VertexVec<VertexSet>, height: &mut VertexVec<usize>, v: Vertex) {
            let mut h = 0;
            for u in covered_by[v].iter() {
                if height[u] == usize::MAX {
                    compute_height_rec(order, covered_by, height, u);
                }
                // So now we do know all those heights
                h = h.max(height[u] + 1);
            }
            height[v] = h;
        }

        let mut heights = VertexVec::new(order, &usize::MAX);
        let mut max_height = 0;
        for v in order.iter_verts() {
            if heights[v] == usize::MAX {
                compute_height_rec(order, &covered_by, &mut heights, v)
            }
            max_height = max_height.max(heights[v]);
        }
        
        Poset {
            order,
            gt,
            covers,
            covered_by,
            downsets,
            upsets,
            heights,
            height: max_height + 1,
            constructor,
        }
    }

    /**
     * First take the transitive closure of the ordering and then make the poset from it.
     * Runs Floyd-Warshall.
     */
    pub fn of_transitive_closure(mut gt: VertexVec<VertexVec<bool>>, constructor: Constructor) -> Poset {
        let order = gt.len();
        for k in order.iter_verts() {
            for i in order.iter_verts() {
                for j in order.iter_verts() {
                    gt[i][j] = gt[i][j] || (gt[i][k] && gt[k][j]);
                }
            }
        }
        Poset::of_ordering(gt, constructor)
    }

    /**
     * Is there a comparability chain from 0 to everywhere else?
     */
    pub fn is_connected(&self) -> bool {
        let mut found = VertexVec::new(self.order, &false);
        let mut q = queue![];
        let _ = q.add(Vertex::ZERO);
        let mut num_found = 1;
        found[Vertex::ZERO] = true;
        while let Ok(next) = q.remove() {
            for u in self.order.iter_verts() {
                if !found[u] && self.comparable(u, next) {
                    found[u] = true;
                    num_found += 1;
                    let _ = q.add(u);
                }
            }
        }
        num_found == self.order.to_usize()
    }

    pub fn incomparable(&self, u: Vertex, v: Vertex) -> bool {
        !self.comparable(u, v)
    }

    pub fn comparable(&self, u: Vertex, v: Vertex) -> bool {
        self.gt[u][v] || self.gt[v][u]
    }

    /**
     * Construct an auxilliary bipartite graph and find a maximal matching.
     * By Dilworth's theorem, this allows us to deduce the size of the largest antichain.
     */
    pub fn get_width(&self) -> u32 {
        let n = self.order.times(2);
        let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
        for (u, v) in self.iter_pairs() {
            if self.gt[u][v] {
                adj[v][u.incr_by_order(self.order)] = true;
            } else if self.gt[v][u] {
                adj[u][v.incr_by_order(self.order)] = true;
            }
        }
        let g = Graph::of_matrix(adj, Constructor::Special);
        let mut dossier = Dossier::new(Entity::Graph(g));
        let mut ann_box = AnnotationsBox::new();
        let operation = crate::operation::int_operation::IntOperation::MaxMatching;
        let max_matching = dossier.operate_int(&mut ann_box, &operation);
        // This is the number of edges in chains; subtracting this from order gives the number of chains.
        self.order.to_usize() as u32 - max_matching
    }

    /**
     * Is the subposet induces by the vs a chain?
     */
    pub fn is_chain(&self, vs: VertexSet) -> bool {
        for x in vs.iter() {
            for y in vs.iter() {
                if x != y && self.incomparable(x, y) {
                    return false
                }
            }
        }
        true
    }

    /**
     * Is the incomparability graph connected?
     */
    pub fn is_incomparability_connected(&self) -> bool {
        let mut found = VertexVec::new(self.order, &false);
        let mut q = queue![];
        let _ = q.add(Vertex::ZERO);
        found[Vertex::ZERO] = true;
        let mut num_found = 1;
        while let Ok(next) = q.remove() {
            for u in self.iter_verts() {
                if self.incomparable(next, u) && !found[u] {
                    found[u] = true;
                    num_found += 1;
                    let _ = q.add(u);
                }
            }
        }
        num_found == self.order.to_usize()
    }

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        self.order.iter_verts()
    }

    pub fn iter_pairs(&self) -> impl Iterator<Item = (Vertex, Vertex)> {
        self.order.iter_pairs()
    }

    pub fn print_hasse(&self) {
        println!("Hasse diagram of the poset:");
        for h in 0..self.height {
            println!("Height = {}", h);
            for v in self.order.iter_verts() {
                if self.heights[v] == h {
                    if self.covers[v].is_empty() {
                        println!("{}", v);
                    } else {
                        print!("{} <", v);
                        for u in self.covers[v].iter() {
                            print!(" {}[{}],", u, self.heights[u]);
                        }
                        println!();
                    }
                }
            }
        }
    }

    pub fn print(&self) {
        println!("{}", self.constructor);
        println!("\nCoverings: ");
        for (v, covers) in self.covers.iter_enum() {
            if covers.is_empty() {
                println!("{}", v);
            } else {
                println!("{} > {:?}", v, covers);
            }
        }
        println!("Order: {}", self.order);
        let rows = self.gt.iter().map(
            |row| ("row!", row.to_vec_of_strings())
        ).collect::<Vec<(&str, VertexVec<String>)>>();
        print_vertex_table(rows);
        self.print_hasse()
    }
}