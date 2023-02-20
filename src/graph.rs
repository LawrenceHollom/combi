use utilities::*;
use crate::constructor::*;
use Constructor::*;
use RawConstructor::*;

use rand::{thread_rng, Rng};
use queues::*;

mod products;
mod erdos_renyi;
mod from_file;
mod grid;
mod random_planar;
mod random_regular_bipartite;
mod tree;
mod raw;
mod bowties;
mod regular;

pub struct Graph {
    pub n: Order,
    pub adj: Vec<Vec<bool>>,
    pub adj_list: Vec<Vec<usize>>,
    pub deg: Vec<Degree>,
    pub constructor: Constructor,
}

impl Graph {
    fn new_complete(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = vec![n-1; n].iter().map(|x| Degree::of_usize(*x)).collect();

        // This could be done directly and non-mutably; possibly would be more idiomatic
        for i in 0..(n-1) {
            for j in (i+1)..n {
                adj[i][j] = true;
                adj[j][i] = true;
            }
        }

        for (i, nbrs) in adj_list.iter_mut().enumerate() {
            for j in 0..n {
                if i != j {
                    nbrs.push(j);
                }
            }
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
            constructor: Raw(Complete(*order))
        }
    }

    fn new_cyclic(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = {
            if n == 1 { vec![0; n] }
            else if n == 2 { vec![1; n] }
            else { vec![2; n]}
        }.iter().map(|x| Degree::of_usize(*x)).collect();

        // This could be done directly and non-mutably; possibly would be more idiomatic
        for i in 0..n {
            adj[i][(i + 1) % n] = true;
            adj[i][(i + n - 1) % n] = true;
            adj_list[i].push((i + 1) % n);
            adj_list[i].push((i + n - 1) % n);
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
            constructor: Raw(Cyclic(*order))
        }
    }

    fn new_path(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = vec![2; n].iter().enumerate().map(|(i, x)| 
                Degree::of_usize(if i == 0 || i == n-1 { 1 } else { *x })).collect();

        // This could be done directly and non-mutably; possibly would be more idiomatic
        for i in 0..n {
            if i + 1 < n {
                adj[i][i + 1] = true;
                adj_list[i].push(i + 1);
            }
            if i >= 1 {
                adj[i][i - 1] = true;
                adj_list[i].push(i - 1);
            }
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
            constructor: Raw(Path(*order))
        }
    }

    fn new_star(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = (0..n).map(|x| Degree::of_usize(if x == 0 { n - 1 } else { 1 })).collect();

        for i in 1..n {
            adj[0][i] = true;
            adj[i][0] = true;
            adj_list[0].push(i);
            adj_list[i].push(0);
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
            constructor: Raw(Star(*order))
        }
    }

    fn new_empty(order: &Order) -> Graph {
        let n = order.to_usize();
        let adj = vec![vec![false; n]; n];
        let adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = vec![0; n].iter().map(|x| Degree::of_usize(*x)).collect();
        
        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
            constructor: Raw(Empty(*order))
        }
    }

    pub fn of_filtered(&self, filter: &[bool]) -> Graph {
        let n = filter.iter().filter(|x| **x).count();
        let mut adj: Vec<Vec<bool>> = vec![(); n].iter().map(|()| vec![false; n]).collect();
        let mut adj_list: Vec<Vec<usize>> = vec![];
        let mut deg = vec![Degree::ZERO; n];

        let mut map = vec![0; n];
        let mut target = 0;
        for map_par in map.iter_mut() {
            while !filter[target] {
                target += 1;
            }
            *map_par = target;
            target += 1;
        }

        for i in 0..n {
            adj_list.push(vec![]);
            for j in 0..n {
                adj[i][j] = self.adj[map[i]][map[j]];
                if adj[i][j] {
                    adj_list[i].push(j);
                    deg[i] = deg[i].incr();
                }
            }
        }

        Graph {
            n: Order::of_usize(n),
            adj,
            adj_list,
            deg,
            constructor: Special,
        }
    }

    pub fn bunkbed(&self) -> Graph {
        let n = self.n.to_usize();
        let mut adj = vec![vec![false; 2*n]; 2*n];
        let mut adj_list = vec![vec![]; 2*n];
        let mut deg: Vec<Degree> = vec![];

        for j in 0..2_usize {
            for i in 0..n {
                deg.push(self.deg[i].incr());
                adj[i + j * n][i + (1 - j) * n] = true; //post
                adj_list[i + j * n].push(i + (1 - j) * n);
                for k in 0..n {
                    if self.adj[i][k] {
                        adj[i + j * n][k + j * n] = true;
                        adj_list[i + j * n].push(k + j * n);
                    }
                }
            }
        }

        Graph {
            n: Order::of_usize(2*n),
            adj,
            adj_list,
            deg,
            constructor: Special
        }
    }

    fn of_adj_list(adj_list: Vec<Vec<usize>>, constructor: Constructor) -> Graph {
        let n: usize = adj_list.len();
        let mut adj = vec![vec![false; n]; n];
        let mut deg = vec![0; n];
    
        for i in 0..n {
            for j in adj_list[i].iter() {
                adj[i][*j] = true;
                deg[i] += 1;
            }
        }
        Graph {
            n: Order::of_usize(n),
            adj,
            adj_list,
            deg: deg.iter().map(|x| Degree::of_usize(*x)).collect(),
            constructor
        }
    }

    fn codeg_code(u: usize, v: usize) -> usize {
        if u < v {
            ((v * (v - 1)) / 2) + u
        } else {
            ((u * (u - 1)) / 2) + v
        }
    }

    pub fn codegree_sequence(&self) -> Vec<usize> {
        let n = self.n.to_usize();
        let mut codegs = vec![0; (n * (n - 1)) / 2];

        for nbrs in self.adj_list.iter() {
            for u in nbrs.iter() {
                for v in nbrs.iter() {
                    if *u < *v {
                        codegs[Self::codeg_code(*u, *v)] += 1;
                    }
                }
            }
        }

        codegs
    }

    fn is_map_isomorphism(&self, g: &Graph, map: &[usize]) -> bool {
        let n = g.n.to_usize();
        let mut is_iso = true;
        'test: for i in 0..n {
            for j in 0..n {
                if self.adj[i][j] != g.adj[map[i]][map[j]] {
                    is_iso = false;
                    break 'test;
                }
            }
        }
        is_iso
    }

    fn is_isomorphic_to_rec(&self, g: &Graph, ordering: &Vec<usize>, map: &mut Vec<usize>, covered: &mut Vec<bool>, 
            node: usize, self_codegs: &Vec<usize>, g_codegs: &Vec<usize>) -> bool {
        let n = g.n.to_usize();
        if node == n {
            self.is_map_isomorphism(g, map)
        } else {
            let mut is_any_iso = false;
            let v = ordering[node];
            // i is the target location of node
            'find_iso: for i in 0..n {
                if self.deg[v] == g.deg[i] && !covered[i] {
                    let mut adj_check = true;
                    'adj_test: for u in self.adj_list[v].iter() {
                        if map[*u] != n && (!g.adj[map[*u]][i]
                                || self_codegs[Self::codeg_code(*u, v)] != g_codegs[Self::codeg_code(map[*u], i)]) {
                            adj_check = false;
                            break 'adj_test;
                        }
                    }
                    if adj_check {
                        map[v] = i;
                        covered[i] = true;
                        if self.is_isomorphic_to_rec(g, ordering, map, covered, node + 1, self_codegs, g_codegs) {
                            is_any_iso = true;
                            break 'find_iso;
                        }
                        map[v] = n;
                        covered[i] = false;
                    }
                }
            }
            is_any_iso
        }
    }

    pub fn is_isomorphic_to(&self, g: &Graph) -> bool {
        let mut self_degs = self.deg.to_owned();
        let mut g_degs = g.deg.to_owned();
        self_degs.sort();
        g_degs.sort();

        if self.n != g.n {
            return false;
        }
        let n = self.n.to_usize();

        if !self_degs.iter().zip(g_degs.iter()).all(|(x, y)| *x == *y) {
            return false;
        }

        let mut self_codegs = self.codegree_sequence();
        let mut g_codegs = g.codegree_sequence();
        self_codegs.sort();
        g_codegs.sort();

        if !self_codegs.iter().zip(g_codegs.iter()).all(|(x, y)| *x == *y) {
            //println!("Gottem! {} ~ {}", self.constructor, g.constructor);
            return false;
        }

        let mut self_comps = self.component_sizes();
        let mut g_comps = g.component_sizes();
        self_comps.sort();
        g_comps.sort();

        if !self_comps.iter().zip(g_comps.iter()).all(|(x, y)| *x == *y) {
            //println!("Connectedness catch!");
            return false;
        }

        if n > 15 { 
            // give up; would be too slow
            return false;
        }

        // BFS on self to find ordering.
        let mut q: Queue<usize> = queue![];
        let mut ordering = vec![];
        let mut visited = vec![false; n];
        for start in 0..n {
            if !visited[start] {
                visited[start] = true;
                let _ = q.add(start);
            }
            'bfs: loop {
                match q.remove() {
                    Ok(node) => {
                        ordering.push(node);
                        for v in self.adj_list[node].iter() {
                            if !visited[*v] {
                                visited[*v] = true;
                                let _ = q.add(*v);
                            }
                        }
                    },
                    Err(_) => break 'bfs,
                }
            }
        }

        let is_iso = self.is_isomorphic_to_rec(g, &ordering, &mut vec![n; n], &mut vec![false; n], 0,
                &self.codegree_sequence(), &g.codegree_sequence());
        if !is_iso {
            println!("Missed: {} !~ {}", self.constructor, g.constructor);
        }
        is_iso
    }

    // Test components, but only considering vertices in the filter.
    pub fn filtered_components(&self, filter: &[bool]) -> Vec<usize> {
        let n = self.n.to_usize();
        let mut comp: Vec<usize> = vec![n; n];
        let mut q: Queue<usize> = queue![];
    
        for i in 0..n {
            if comp[i] == n && filter[i] {
                comp[i] = i;
                let _ = q.add(i);
                'flood_fill: loop {
                    match q.remove() {
                        Ok(node) => {
                            for j in self.adj_list[node].iter() {
                                if comp[*j] == n && filter[*j] {
                                    comp[*j] = i;
                                    let _ = q.add(*j);
                                }
                            }
                        },
                        Err(_err) => break 'flood_fill,
                    }
                }
            }
        }
    
        comp
    }
    
    pub fn components(&self) -> Vec<usize> {
        self.filtered_components(&vec![true; self.n.to_usize()])
    }
    
    pub fn largest_component(&self) -> u32 {
        let n = self.n.to_usize();
        let comps = self.components();
    
        let mut sizes: Vec<u32> = vec![0; n];
    
        for comp in comps.iter() {
            sizes[*comp] += 1;
        }
    
        let mut max = 0;
        for size in sizes.iter() {
            if *size > max {
                max = *size;
            }
        }
    
        max
    }
    
    pub fn is_connected(&self) -> bool {
        self.largest_component() == self.n.to_usize() as u32
    }

    pub fn is_regular(&self) -> bool {
        self.max_degree() == self.min_degree()
    }
    
    pub fn num_filtered_components(&self, filter: &[bool]) -> u32 {
        let n = self.n.to_usize();
        let comps = self.filtered_components(filter);
    
        let mut is_comp: Vec<bool> = vec![false; n];
        let mut num_comps: u32 = 0;
    
        for (i, comp) in comps.iter().enumerate() {
            if filter[i] && !is_comp[*comp] {
                num_comps += 1;
                is_comp[*comp] = true;
            }
        }
    
        num_comps
    }

    pub fn num_components(&self) -> u32 {
        self.num_filtered_components(&vec![true; self.n.to_usize()])
    }

    fn component_sizes(&self) -> Vec<usize> {
        let n = self.n.to_usize();
        let mut num_components = 0;
        let mut labels = vec![n; n];
        let mut sizes = vec![];
        for comp in self.components() {
            if labels[comp] == n {
                labels[comp] = num_components;
                num_components += 1;
                sizes.push(1)
            } else {
                sizes[labels[comp]] += 1;
            }
        }

        sizes
    }

    pub fn new(constructor: &Constructor) -> Graph {
        use Constructor::*;
        use RandomConstructor::*;
        use RawConstructor::*;

        match constructor {
            Product(product, c1, c2) => {
                products::new_product(product, constructor, &Self::new(c1), &Self::new(c2))
            }
            RootedTree(parents) => tree::new_rooted(parents),
            Random(Biregular(order, left_deg, right_deg)) => {
                random_regular_bipartite::new_biregular(order, left_deg, right_deg)
            }
            Random(ErdosRenyi(order, p)) => erdos_renyi::new(order, *p),
            Random(Triangulation(order)) => random_planar::new_triangulation(order),
            Random(MaximalPlanar(order)) => random_planar::new_maximal(order),
            Random(Bowties(scale, degree)) => bowties::new_bowties(*scale, *degree),
            Random(Regular(order, degree)) => regular::new_regular(order, degree),
            Raw(Grid(height, width)) => grid::new(height, width),
            Raw(Complete(order)) => Graph::new_complete(order),
            Raw(Cyclic(order)) => Graph::new_cyclic(order),
            Raw(Path(order)) => Graph::new_path(order),
            Raw(Star(order)) => Graph::new_star(order),
            Raw(Empty(order)) => Graph::new_empty(order),
            Raw(Cube(dimension)) => raw::new_cube(*dimension),
            Raw(FanoPlane) => raw::new_fano_plane(),
            Raw(Petersen(cycles, skip)) => raw::new_petersen(*cycles, *skip),
            Raw(Octahedron) => raw::new_octahedron(),
            Raw(Icosahedron) => raw::new_icosahedron(),
            Raw(Dodecahedron) => raw::new_dodecahedron(),
            File(filename) => from_file::new_graph(filename),
            Special => panic!("Cannot directly construct Special graph!"),
        }
    }

    pub fn complement(&self) -> Graph {
        let n = self.n.to_usize();
        let mut new_adj = vec![vec![false; n]; n];
        let mut new_adj_list = vec![vec![n-1]; n];
        let new_deg: Vec<Degree> = self.deg.iter().map(|x| Degree::of_usize(n - x.to_usize() - 1)).collect();

        for i in 0..(n-1) {
            for j in (i+1)..n {
                new_adj[i][j] = !self.adj[i][j];
                new_adj[j][i] = !self.adj[j][i];
            }
        }
        
        for i in 0..n {
            for j in 0..n {
                if new_adj[i][j] {
                    new_adj_list[i].push(j);
                }
            }
        }

        Graph {
            n: self.n,
            adj: new_adj,
            adj_list: new_adj_list,
            deg: new_deg,
            constructor: Constructor::Special,
        }
    }

    pub fn print(&self) {
        let n: usize = self.n.to_usize();
        println!("n: {}", n);
        println!("degs: {:?}", self.deg.iter().map(|x| x.to_usize()).collect::<Vec<usize>>());
        for i in 0..(n-1) {
            for j in (i+1)..n {
                if self.adj[i][j] {
                    println!("{} ~ {}", i, j);
                }
            }
        }
    }

    pub fn print_matrix(&self) {
        let n = self.n.to_usize();
        println!("n: {}, m: {}", n, self.size());
        for i in 0..n {
            for j in 0..n {
                if self.adj[i][j] {
                    print!("1");
                } else {
                    print!("0");
                }
            }
            println!();
        }
    }

    pub fn size(&self) -> usize {
        self.deg.iter().fold(0, |accum, val| accum + val.to_usize()) / 2
    }

    pub fn filtered_size(&self, filter: &[bool]) -> usize {
        self.adj_list
            .iter()
            .enumerate()
            .fold(0, |accum, (i, adjs)|
                if filter[i] {
                    accum + adjs.iter().filter(|x| filter[**x]).count()
                } else {
                    accum
                }
            ) / 2
    }

    pub fn filtered_degree(&self, v: usize, filter: &[bool]) -> usize {
        self.adj_list[v].iter().filter(|x| filter[**x]).count()
    }

    pub fn min_degree(&self) -> u32 {
        self.deg.iter().map(|x| x.to_usize()).min().unwrap() as u32
    }

    pub fn max_degree(&self) -> u32 {
        self.deg.iter().map(|x| x.to_usize()).max().unwrap() as u32
    }
}