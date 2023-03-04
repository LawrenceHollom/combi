use std::slice::Iter;

use utilities::{*, edge_tools::*, vertex_tools::*};
use crate::constructor::*;
use Constructor::*;
use RawConstructor::*;

use rand::{thread_rng, Rng, seq::SliceRandom};
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
    pub adj: VertexVec<VertexVec<bool>>,
    pub adj_list: VertexVec<Vec<Vertex>>,
    pub deg: VertexVec<Degree>,
    pub constructor: Constructor,
}

impl Graph {
    pub fn of_adj_list(adj_list: VertexVec<Vec<Vertex>>, constructor: Constructor) -> Graph {
        let n = adj_list.len();
        let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
        let mut deg = VertexVec::new_fn(n, |_| Degree::ZERO);
        let mut new_adj_list = VertexVec::new_fn(n, |_| vec![]);
    
        for i in n.iter_verts() {
            for j in adj_list[i].iter() {
                adj[i][*j] = true;
                new_adj_list[i].push(*j);
                deg[i].incr_inplace();
            }
        }
        Graph {
            n,
            adj,
            adj_list: new_adj_list,
            deg,
            constructor
        }
    }

    fn new_complete(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for i in n.iter_verts() {
            for j in n.iter_verts() {
                if i != j {
                    adj_list[i].push(j);
                }
            }
        }

        Graph::of_adj_list(adj_list, Raw(Complete(n)))
    }

    fn new_cyclic(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for i in n.iter_verts() {
            adj_list[i].push(i.incr(n));
            adj_list[i].push(i.decr(n));
        }

        Graph::of_adj_list(adj_list, Raw(Cyclic(n)))
    }

    fn new_path(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for i in n.iter_verts().take(n.to_usize() - 1) {
            adj_list[i].push(i.incr(n));
        }
        for i in n.iter_verts().skip(1) {
            adj_list[i].push(i.decr(n));
        }

        Graph::of_adj_list(adj_list, Raw(Path(n)))
    }

    fn new_star(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);
        let zero = Vertex::of_usize(0);

        for i in n.iter_verts().skip(1) {
            adj_list[zero].push(i);
            adj_list[i].push(zero);
        }

        Graph::of_adj_list(adj_list, Raw(Star(n)))
    }

    fn new_empty(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);
        
        Graph::of_adj_list(adj_list, Raw(Empty(n)))
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
        let new_n = self.n + self.n;
        let mut adj_list = VertexVec::new_fn(&new_n, |_| vec![]);
        let nu = self.n.to_usize();

        for j in 0..2_usize {
            for i in 0..nu {
                adj_list[Vertex::of_usize(i + j * nu)].push(Vertex::of_usize(i + (1 - j) * nu)); //post
                for k in 0..nu {
                    if self.adj[Vertex::of_usize(i)][Vertex::of_usize(k)] {
                        adj_list[Vertex::of_usize(i + j * nu)].push(Vertex::of_usize(k + j * nu));
                    }
                }
            }
        }

        Graph::of_adj_list(adj_list, Special)
    }

    fn codeg_code(u: Vertex, v: Vertex) -> usize {
        Edge::of_pair(u, v).encode()
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

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        (0..self.n.to_usize()).map(|x| Vertex::of_usize(x))
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

    pub fn is_adj_commutative(&self) -> bool {
        let mut is_comm = true;
        'test_is_comm: for (u, v) in self.n.iter_pairs() {
            if self.adj[u][v] ^ self.adj[v][u] {
                is_comm = false;
                break 'test_is_comm;
            }
        }
        is_comm
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

    fn permute_vertices(&self, ordering: &VertexVec<Vertex>) -> (Graph, VertexVec<Vertex>) {
        let mut ordering_inv: VertexVec<Vertex> = VertexVec::new(self.n, &Vertex::ZERO);

        for i in self.iter_verts() {
            ordering_inv[ordering[i]] = i;
        }

        let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(self.n, &vec![]);
        for u in self.iter_verts() {
            for v in self.adj_list[ordering[u]].iter() {
                adj_list[u].push(ordering_inv[*v]);
            }
        }
        let g = Graph::of_adj_list(adj_list, self.constructor.to_owned());
        (g, ordering_inv)
    }

    // Returns an isomorphic version of self with vertices ordered by:
    // - Take a vertex u of min unpushed degree; push.
    // - Push all nbrs of u.
    // - Repeat until all vertices pushed.
    //
    // This will be used to try and speed up some NP-hard search algos.
    pub fn order_by_nbhd(&self) -> (Graph, Vec<usize>) {
        let n = self.n.to_usize();
        let mut placed = vec![false; n];
        let mut ordering: Vec<usize> = vec![];
        let mut num_placed = 0;
        while num_placed < n {
            let mut min_unpushed_deg = n;
            let mut best_vert = n;
            for u in 0..n { 
                if !placed[u] {
                    let mut deg = 0;
                    for v in self.adj_list[u].iter() {
                        if !placed[*v] {
                            deg += 1;
                        }
                    }
                    if deg < min_unpushed_deg {
                        min_unpushed_deg = deg;
                        best_vert = u;
                    }
                }
            }
            placed[best_vert] = true;
            ordering.push(best_vert);
            num_placed += 1;
            for v in self.adj_list[best_vert].iter() {
                if !placed[*v] {
                    placed[*v] = true;
                    ordering.push(*v);
                    num_placed += 1;
                }
            }
        }
        self.permute_vertices(&ordering)
    }

    pub fn randomly_permute_vertices(&self) -> (Graph, VertexVec<Vertex>) {
        let mut rng = thread_rng();
        let mut ordering: VertexVec<Vertex> = self.n.iter_verts().collect();
        ordering.shuffle(&mut rng);
        self.permute_vertices(&ordering)
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
            Random(PlanarConditioned(order, max_deg, min_girth)) => {
                random_planar::new_conditioned(order, *max_deg, *min_girth)
            }
            Random(PlanarGons(order, k)) => random_planar::k_gon_gluing(order, *k),
            Random(Bowties(scale, degree)) => bowties::new_bowties(*scale, *degree),
            Random(Regular(order, degree)) => regular::new_regular(order, degree),
            Random(DegreeSequence(deg_seq)) => regular::new_from_degree_sequence(deg_seq, false),
            Random(VertexStructured(pattern, num)) => pattern.new_graph(*num),
            Random(EdgeStructured(pattern, num)) => pattern.new_graph(*num),
            Raw(Grid(height, width)) => grid::new(height, width),
            Raw(Complete(order)) => Graph::new_complete(*order),
            Raw(Cyclic(order)) => Graph::new_cyclic(*order),
            Raw(Path(order)) => Graph::new_path(*order),
            Raw(Star(order)) => Graph::new_star(*order),
            Raw(Empty(order)) => Graph::new_empty(*order),
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

    pub fn floyd_warshall(&self) -> Vec<Vec<usize>> {
        let n = self.n.to_usize();
        let mut dist = vec![vec![n; n]; n];
        for (i, d) in dist.iter_mut().enumerate() {
            d[i] = 0;
        }
        for (i, d) in dist.iter_mut().enumerate() {
            for j in self.adj_list[i].iter() {
                d[*j] = 1;
            }
        }
        for k in 0..n {
            for i in 0..n {
                for j in 0..n {
                    if dist[i][j] > dist[i][k] + dist[k][j] {
                        dist[i][j] = dist[i][k] + dist[k][j];
                    }
                }
            }
        }
        dist
    }

    pub fn delete_edge(&mut self, e: Edge) {
        let x = e.fst();
        let y = e.snd();
        if self.adj[x][y] {
            self.adj[x][y] = false;
            self.adj[y][x] = false;
            'find_y: for (i, z) in self.adj_list[x].iter().enumerate() {
                if *z == y {
                    self.adj_list[x].swap_remove(i);
                    break 'find_y;
                }
            }
            'find_x: for (i, z) in self.adj_list[y].iter().enumerate() {
                if *z == x {
                    self.adj_list[y].swap_remove(i);
                    break 'find_x;
                }
            }
            self.deg[x] = self.deg[x].decr();
            self.deg[y] = self.deg[y].decr();
        }
    }

    pub fn reduce_max_degree(&mut self, max_deg: Degree) {
        'cut_edges: loop {
            let mut are_degs_good = true;
            'test_degs: for d in self.deg.iter() {
                if d > &max_deg {
                    are_degs_good = false;
                    break 'test_degs;
                }
            }
            if are_degs_good {
                break 'cut_edges;
            }

            let mut max_edge = Edge::of_pair(0, 1);
            let mut max_deg = Degree::ZERO;
            for (u, adj) in self.adj_list.iter().enumerate() {
                for v in adj.iter() {
                    if self.deg[u] + self.deg[*v] > max_deg {
                        max_deg = self.deg[u] + self.deg[*v];
                        max_edge = Edge::of_pair(u, *v);
                    }
                }
            }
            self.delete_edge(max_edge);
        }
    }

    fn list_cycles_rec(&self, dist: &Vec<Vec<usize>>, visited: &mut Vec<bool>, cycle: &mut Vec<Edge>, 
            current_len: usize, target_len: usize, this_vert: usize, start_vert: usize, list: &mut Vec<Vec<Edge>>) {
        if current_len == target_len - 1 {
            if self.adj[this_vert][start_vert] {
                cycle.push(Edge::of_pair(this_vert, start_vert));
                list.push(cycle.to_owned());
                let _ = cycle.pop();
            }
        } else {
            // Try adding more vertices.
            for v in self.adj_list[this_vert].iter() {
                if !visited[*v] && dist[start_vert][*v] < target_len - current_len {
                    visited[*v] = true;
                    cycle.push(Edge::of_pair(this_vert, *v));
                    self.list_cycles_rec(dist, visited, cycle, current_len + 1, target_len, *v, start_vert, list);
                    let _ = cycle.pop();
                    visited[*v] = false;
                }
            }
        }
    }

    pub fn remove_all_k_cycles(&mut self, k: usize) {
        let n = self.n.to_usize();
    
        let dist = self.floyd_warshall();

        // DFS to find all cycles.
        let mut list: Vec<Vec<Edge>> = vec![];
        for u in 0..n {
            let mut visited = vec![false; n];
            visited[u] = true;
            self.list_cycles_rec(&dist, &mut visited, &mut vec![],
                0, k, u, u, &mut list);
        }
        // Now remove edges to kill the cycles.
        let mut removed = EdgeVec::new(&self.adj_list, false);
        for cycle in list {
            let mut still_there = true;
            'test_cycle: for e in cycle.iter() {
                if removed.get(*e) {
                    still_there = false;
                    break 'test_cycle;
                }
            }
            if still_there {
                // Remove the edge of largest deg_sum.
                let mut max_sum = Degree::ZERO;
                let mut max_edge = cycle[0];
                for e in cycle.iter() {
                    let ds = self.deg[e.fst()] + self.deg[e.snd()];
                    if ds > max_sum {
                        max_sum = ds;
                        max_edge = *e;
                    }
                }
                // Remove max_edge
                removed.set(max_edge, true);
                self.delete_edge(max_edge);

            }
        }
    }
}