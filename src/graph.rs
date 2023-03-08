use utilities::{*, edge_tools::*, vertex_tools::*, component_tools::*};
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

        for (i, j) in n.iter_pairs() {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }

        Graph::of_adj_list(adj_list, Raw(Complete(n)))
    }

    fn new_complete_bipartite(left: Order, right: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(left + right, |_| vec![]);

        for j in right.iter_verts() {
            let k = j.incr_by_order(left);
            for i in left.iter_verts() {
                adj_list[i].push(k);
                adj_list[k].push(i);
            }
        }

        Graph::of_adj_list(adj_list, Raw(CompleteBipartite(left, right)))
    }

    fn new_cyclic(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for i in n.iter_verts() {
            adj_list[i].push(i.incr_wrap(n));
            adj_list[i].push(i.decr_wrap(n));
        }

        Graph::of_adj_list(adj_list, Raw(Cyclic(n)))
    }

    fn new_path(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for i in n.iter_verts().take(n.to_usize() - 1) {
            adj_list[i].push(i.incr());
        }
        for i in n.iter_verts().skip(1) {
            adj_list[i].push(i.decr());
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
        let adj_list = VertexVec::new_fn(n, |_| vec![]);
        
        Graph::of_adj_list(adj_list, Raw(Empty(n)))
    }

    pub fn of_filtered(&self, filter: &VertexVec<bool>) -> Graph {
        let n = Order::of_usize(filter.iter().filter(|x| **x).count());
        let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(n, &vec![]);

        let mut map = VertexVec::new(n, &Vertex::ZERO);
        let mut target = Vertex::ZERO;
        for map_par in map.iter_mut() {
            while !filter[target] {
                target.incr_inplace();
            }
            *map_par = target;
            target.incr_inplace();
        }

        for (i, j) in n.iter_pairs() {
            if self.adj[map[i]][map[j]] {
                adj_list[i].push(j);
                adj_list[j].push(i);
            }
        }

        Graph::of_adj_list(adj_list, Special)
    }

    pub fn bunkbed(&self) -> Graph {
        let new_n = self.n + self.n;
        let mut adj_list = VertexVec::new_fn(new_n, |_| vec![]);
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

    fn is_map_isomorphism(&self, g: &Graph, map: &VertexVec<Option<Vertex>>) -> bool {
        for (i, j) in self.n.iter_pairs() {
            match (map[i], map[j]) {
                (Some(x), Some(y)) => {
                    if self.adj[i][j] != g.adj[x][y] {
                        return false;
                    }
                }
                (Some(_), None) | (None, Some(_)) => return false,
                (None, None) => return false,

            }
        }
        true
    }

    fn is_isomorphic_to_rec(&self, g: &Graph, ordering: &VertexVec<Vertex>, map: &mut VertexVec<Option<Vertex>>, 
            covered: &mut VertexVec<bool>, node: Vertex, self_codegs: &Vec<usize>, g_codegs: &Vec<usize>) -> bool {
        if node.is_n(self.n) {
            self.is_map_isomorphism(g, map)
        } else {
            let mut is_any_iso = false;
            let v = ordering[node];
            // i is the target location of node
            'find_iso: for i in self.n.iter_verts() {
                if self.deg[v] == g.deg[i] && !covered[i] {
                    let mut adj_check = true;
                    'adj_test: for u in self.adj_list[v].iter() {
                        if map[*u].map_or(false, |x| !g.adj[x][i]
                                || self_codegs[Self::codeg_code(*u, v)] != g_codegs[Self::codeg_code(x, i)]) {
                            adj_check = false;
                            break 'adj_test;
                        }
                    }
                    if adj_check {
                        map[v] = Some(i);
                        covered[i] = true;
                        if self.is_isomorphic_to_rec(g, ordering, map, covered, node.incr(), self_codegs, g_codegs) {
                            is_any_iso = true;
                            break 'find_iso;
                        }
                        map[v] = None;
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
        self_degs.sort(Degree::cmp);
        g_degs.sort(Degree::cmp);

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
        let mut q: Queue<Vertex> = queue![];
        let mut ordering = VertexVec::new(self.n, &Vertex::ZERO);
        let mut visited = VertexVec::new(self.n, &false);
        let mut next_preimage = Vertex::ZERO;
        for start in self.n.iter_verts() {
            if !visited[start] {
                visited[start] = true;
                let _ = q.add(start);
            }
            'bfs: loop {
                match q.remove() {
                    Ok(node) => {
                        ordering[next_preimage] = node;
                        next_preimage.incr_inplace();
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

        let is_iso = self.is_isomorphic_to_rec(g, &ordering, &mut VertexVec::new(self.n, &None), &mut VertexVec::new(self.n, &false), Vertex::ZERO,
                &self.codegree_sequence(), &g.codegree_sequence());
        if !is_iso {
            println!("Missed: {} !~ {}", self.constructor, g.constructor);
        }
        is_iso
    }

    // Test components, but only considering vertices in the filter.
    pub fn filtered_components(&self, filter: Option<&VertexVec<bool>>) -> VertexVec<Component> {
        let mut comp: VertexVec<Option<Vertex>> = VertexVec::new(self.n, &None);
        let mut q: Queue<Vertex> = queue![];
    
        for i in self.n.iter_verts() {
            if comp[i].is_none() {
                comp[i] = Some(i);
                if filter.map_or(true, |f| f[i]) {
                    let _ = q.add(i);
                    'flood_fill: loop {
                        match q.remove() {
                            Ok(node) => {
                                for j in self.adj_list[node].iter() {
                                    if comp[*j].is_none() && filter.map_or(true, |f| f[*j]) {
                                        comp[*j] = Some(i);
                                        let _ = q.add(*j);
                                    }
                                }
                            },
                            Err(_err) => break 'flood_fill,
                        }
                    }
                }
            }
        }
    
        comp.iter().map(|x| Component::of_vertex(x.unwrap())).collect::<VertexVec<Component>>()
    }
    
    pub fn components(&self) -> VertexVec<Component> {
        self.filtered_components(None)
    }
    
    pub fn largest_component(&self) -> u32 {
        let comps = self.components();
    
        let mut sizes: VertexVec<u32> = VertexVec::new(self.n, &0);
    
        for comp in comps.iter() {
            sizes[comp.to_vertex()] += 1;
        }
    
        *sizes.max(&0, u32::cmp).unwrap()
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
    
    pub fn num_filtered_components(&self, filter: Option<&VertexVec<bool>>) -> u32 {
        let comps = self.filtered_components(filter);
    
        let mut is_comp: VertexVec<bool> = VertexVec::new(self.n, &false);
        let mut num_comps: u32 = 0;
    
        for (i, comp) in comps.iter_enum() {
            if filter.map_or(true, |f| f[i]) && !is_comp[comp.to_vertex()] {
                num_comps += 1;
                is_comp[comp.to_vertex()] = true;
            }
        }
    
        num_comps
    }

    pub fn num_components(&self) -> u32 {
        self.num_filtered_components(None)
    }

    fn component_sizes(&self) -> Vec<usize> {
        let mut num_components = 0;
        let mut labels = VertexVec::new(self.n, &None);
        let mut sizes = vec![];
        for comp in self.components().iter() {
            match labels[comp.to_vertex()] {
                Some(label) => {
                    sizes[label] += 1;
                }
                None => {
                    labels[comp.to_vertex()] = Some(num_components);
                    num_components += 1;
                    sizes.push(1)
                }
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
    pub fn order_by_nbhd(&self) -> (Graph, VertexVec<Vertex>) {
        let mut placed = VertexVec::new(self.n, &false);
        let mut ordering: VertexVec<Vertex> = VertexVec::new(self.n, &Vertex::ZERO);
        let mut num_placed = 0;
        while num_placed < self.n.to_usize() {
            let mut min_unpushed_deg = Degree::INF;
            let mut best_vert = Vertex::ZERO;
            for u in self.n.iter_verts() { 
                if !placed[u] {
                    let mut deg = Degree::ZERO;
                    for v in self.adj_list[u].iter() {
                        if !placed[*v] {
                            deg.incr_inplace();
                        }
                    }
                    if deg < min_unpushed_deg {
                        min_unpushed_deg = deg;
                        best_vert = u;
                    }
                }
            }
            placed[best_vert] = true;
            ordering[Vertex::of_usize(num_placed)] = best_vert;
            num_placed += 1;
            for v in self.adj_list[best_vert].iter() {
                if !placed[*v] {
                    placed[*v] = true;
                    ordering[Vertex::of_usize(num_placed)] = *v;
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

    pub fn flood_fill(&self, start: Vertex, end: Option<Vertex>, filter: Option<&VertexVec<bool>>) -> VertexVec<Option<Vertex>> {
        let mut prev = VertexVec::new(self.n, &None);
        let mut q: Queue<Vertex> = queue![];
        prev[start] = Some(start);
        let _ = q.add(start);
        'flood_fill: loop {
            match q.remove() {
                Ok(node) => {
                    for next in self.adj_list[node].iter() {
                        if node != start && end.map_or(false, |x| *next == x) {
                            prev[end.unwrap()] = Some(node);
                            break 'flood_fill;
                        } else if prev[*next].is_none() && filter.map_or(true, |f| f[*next]) {
                            prev[*next] = Some(node);
                            let _ = q.add(*next);
                        }
                    }
                }
                Err(e) => {
                    // This has caused a crash.
                    self.print();
                    panic!("AAAAAAAAAA {}", e)
                }
            }
        }
        prev
    }

    pub fn flood_fill_dist(&self, start: Vertex) -> VertexVec<Option<u32>> {
        let mut connected = VertexVec::new(self.n, &None);
        let mut q: Queue<Vertex> = queue![];
        connected[start] = Some(0);
        let _ = q.add(start);
        while q.size() > 0 {
            let node = q.remove().unwrap();
            for v in self.adj_list[node].iter() {
                if connected[*v].is_none() {
                    connected[*v] = connected[node].map(|x| x + 1);
                    let _ = q.add(*v);
                }
            }
        }
        connected
    }

    pub fn flood_fill_two_colourable(&self, edge_filter: &EdgeSet, indexer: &EdgeSetIndexer) -> bool {
        let mut colour = VertexVec::new(self.n, &None);
        let mut q: Queue<Vertex> = queue![];
        let mut is_two_colourable = true;
        'test_verts: for v in self.n.iter_verts() {
            if colour[v].is_none() {
                colour[v] = Some(0);
                let _ = q.add(v);
                while q.size() > 0 {
                    let u = q.remove().unwrap();
                    for w in self.adj_list[u].iter() {
                        if edge_filter.has_edge(Edge::of_pair(u, *w), indexer) {
                            if colour[u] == colour[*w] {
                                is_two_colourable = false;
                                break 'test_verts;
                            } else if colour[*w].is_none() {
                                colour[*w] = Some(!colour[u].unwrap());
                                let _ = q.add(*w);
                            }
                        }
                    }
                }
            }
        }
        is_two_colourable
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
                random_regular_bipartite::new_biregular(*order, *left_deg, *right_deg)
            }
            Random(ErdosRenyi(order, p)) => erdos_renyi::new(*order, *p),
            Random(Triangulation(order)) => random_planar::new_triangulation(*order),
            Random(MaximalPlanar(order)) => random_planar::new_maximal(*order),
            Random(PlanarConditioned(order, max_deg, min_girth)) => {
                random_planar::new_conditioned(*order, *max_deg, *min_girth)
            }
            Random(PlanarGons(order, k)) => random_planar::k_gon_gluing(*order, *k),
            Random(Bowties(scale, degree)) => bowties::new_bowties(*scale, *degree),
            Random(Regular(order, degree)) => regular::new_regular(order, degree),
            Random(DegreeSequence(deg_seq)) => regular::new_from_degree_sequence(deg_seq, false),
            Random(VertexStructured(pattern, num)) => pattern.new_graph(*num),
            Random(EdgeStructured(pattern, num)) => pattern.new_graph(*num),
            Raw(Grid(height, width)) => grid::new(height, width),
            Raw(Complete(order)) => Graph::new_complete(*order),
            Raw(CompleteBipartite(left, right)) => Graph::new_complete_bipartite(*left, *right),
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
        let mut new_adj_list = VertexVec::new(self.n, &vec![]);

        for (i, j) in self.n.iter_pairs() {
            if !self.adj[i][j] {
                new_adj_list[j].push(i);
                new_adj_list[i].push(j);
            }
        }

        Graph::of_adj_list(new_adj_list, Special)
    }

    pub fn print(&self) {
        println!("n: {}", self.n);
        println!("degs: {:?}", self.deg.iter().map(|x| x.to_usize()).collect::<Vec<usize>>());
        for (i, j) in self.n.iter_pairs() {
            if self.adj[i][j] {
                println!("{} ~ {}", i, j);
            }
        }
    }

    pub fn print_matrix(&self) {
        println!("n: {}, m: {}", self.n, self.size());
        for i in self.n.iter_verts() {
            for j in self.n.iter_verts() {
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

    pub fn filtered_size(&self, filter: &VertexVec<bool>) -> usize {
        self.adj_list
            .iter_enum()
            .fold(0, |accum, (i, adjs)|
                if filter[i] {
                    accum + adjs.iter().filter(|x| filter[**x]).count()
                } else {
                    accum
                }
            ) / 2
    }

    pub fn filtered_degree(&self, v: Vertex, filter: &VertexVec<bool>) -> usize {
        self.adj_list[v].iter().filter(|x| filter[**x]).count()
    }

    pub fn min_degree(&self) -> u32 {
        self.deg.iter().map(|x| x.to_usize()).min().unwrap() as u32
    }

    pub fn max_degree(&self) -> u32 {
        self.deg.iter().map(|x| x.to_usize()).max().unwrap() as u32
    }

    pub fn floyd_warshall(&self) -> VertexVec<VertexVec<usize>> {
        let mut dist = VertexVec::new(self.n, &VertexVec::new(self.n, &usize::MAX));
        for (i, d) in dist.iter_mut_enum() {
            d[i] = 0;
        }
        for (i, d) in dist.iter_mut_enum() {
            for j in self.adj_list[i].iter() {
                d[*j] = 1;
            }
        }
        for k in self.n.iter_verts() {
            for i in self.n.iter_verts() {
                for j in self.n.iter_verts() {
                    if dist[i][k] < usize::MAX && dist[k][j] < usize::MAX && dist[i][j] > dist[i][k] + dist[k][j] {
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

            let mut max_edge = Edge::FILLER;
            let mut max_deg = Degree::ZERO;
            for (u, adj) in self.adj_list.iter_enum() {
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

    fn list_cycles_rec(&self, dist: &VertexVec<VertexVec<usize>>, visited: &mut VertexVec<bool>, cycle: &mut Vec<Edge>, 
            current_len: usize, target_len: usize, this_vert: Vertex, start_vert: Vertex, list: &mut Vec<Vec<Edge>>) {
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
        let dist = self.floyd_warshall();

        // DFS to find all cycles.
        let mut list: Vec<Vec<Edge>> = vec![];
        for u in self.n.iter_verts() {
            let mut visited = VertexVec::new(self.n, &false);
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

    pub fn iter_edge_sets(&self) -> EdgeSetIterator {
        EdgeSetIterator::new(&self.adj_list)
    }

    #[allow(dead_code)]
    pub fn test_graph(index: usize) -> Graph {
        from_file::new_graph(&format!("test_{}", index))
    }
}

pub fn adj_list_of_manual(adj_list: Vec<Vec<usize>>) -> VertexVec<Vec<Vertex>> {
    adj_list.iter()
        .map(|l| l.iter()
            .map(|x| Vertex::of_usize(*x))
            .collect())
        .collect()
}