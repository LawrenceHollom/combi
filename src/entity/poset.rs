mod random_construction;

use queues::*;
use std::usize;

use utilities::*;
use utilities::vertex_tools::*;

use crate::constructor::*;

#[derive(Clone)]
pub struct Poset {
    pub order: Order,
    gt: VertexVec<VertexVec<bool>>,
    pub covers: VertexVec<Vec<Vertex>>,
    pub covered_by: VertexVec<Vec<Vertex>>,
    pub height: VertexVec<usize>,
    max_height: usize,
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
        let mut covers = VertexVec::new(order, &vec![]);
        let mut covered_by = VertexVec::new(order, &vec![]);
        let mut height = VertexVec::new(order, &0);
        for (i, v) in order.iter_verts().enumerate() {
            if v != Vertex::ZERO {
                covers[v].push(v.decr());
            }
            if v != order.to_max_vertex() {
                covered_by[v].push(v.incr())
            }
            height[v] = i
        }
        Poset {
            order,
            gt,
            covers,
            covered_by,
            height,
            max_height: order.to_usize() - 1,
            constructor: Constructor::PosetConstr(PosetConstructor::Chain(order))
        }
    }

    pub fn new_antichain(order: Order) -> Poset {
        Poset {
            order,
            gt: VertexVec::new(order, &VertexVec::new(order, &false)),
            covers: VertexVec::new(order, &vec![]),
            covered_by: VertexVec::new(order, &vec![]),
            height: VertexVec::new(order, &0),
            max_height: 0,
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
        let mut covers = VertexVec::new(order, &vec![]);
        let mut covered_by = VertexVec::new(order, &vec![]);

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
                covered_by[u].push(v);
                covers[v].push(u);
            }
            if gt[v][u] && upsets[u].inter(&downsets[v]).is_empty() {
                covers[u].push(v);
                covered_by[v].push(u);
            }
        }
        
        fn compute_height_rec(order: Order, covered_by: &VertexVec<Vec<Vertex>>, height: &mut VertexVec<usize>, v: Vertex) {
            let mut h = 0;
            for u in covered_by[v].iter() {
                if height[*u] == usize::MAX {
                    compute_height_rec(order, covered_by, height, *u);
                }
                // So now we do know all those heights
                h = h.max(height[*u] + 1);
            }
            height[v] = h;
        }

        let mut height = VertexVec::new(order, &usize::MAX);
        let mut max_height = 0;
        for v in order.iter_verts() {
            if height[v] == usize::MAX {
                compute_height_rec(order, &covered_by, &mut height, v)
            }
            max_height = max_height.max(height[v]);
        }
        
        Poset {
            order,
            gt,
            covers,
            covered_by,
            height,
            max_height,
            constructor,
        }
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
            for u in self.covered_by[next].iter() {
                if !found[*u] {
                    found[*u] = true;
                    num_found += 1;
                    let _ = q.add(*u);
                }
            }
            for u in self.covers[next].iter() {
                if !found[*u] {
                    found[*u] = true;
                    num_found += 1;
                    let _ = q.add(*u);
                }
            }
        }
        num_found == self.order.to_usize()
    }

    pub fn incomp(&self, u: Vertex, v: Vertex) -> bool {
        !(self.gt[u][v] || self.gt[v][u])
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
                if self.incomp(next, u) && !found[u] {
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
        for h in 0..=self.max_height {
            println!("Height = {}", h);
            for v in self.order.iter_verts() {
                if self.height[v] == h {
                    if self.covers[v].is_empty() {
                        println!("{}", v);
                    } else {
                        print!("{} <", v);
                        for u in self.covers[v].iter() {
                            print!(" {}[{}],", u, self.height[*u]);
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