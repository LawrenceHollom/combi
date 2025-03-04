#![allow(dead_code)]

use utilities::{*, edge_tools::*, vertex_tools::*, component_tools::*};
use crate::constructor::*;
use Constructor::*;
use RawConstructor::*;

use rand::thread_rng;
use queues::*;

mod flood_fill;
mod isomorphisms;
mod cycles;

#[derive(Clone)]
pub struct Graph {
    pub n: Order,
    pub adj: VertexVec<VertexVec<bool>>,
    pub adj_list: VertexVec<Vec<Vertex>>,
    pub deg: VertexVec<Degree>,
    pub constructor: Constructor,
}

pub struct EdgeIterator<'a> {
    g: &'a Graph,
    sub_iter: VertexPairIterator,
}

impl Graph {
    pub fn of_adj_list(adj_list: VertexVec<Vec<Vertex>>, constructor: Constructor) -> Graph {
        let n = adj_list.len();
        let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
        let deg = adj_list.iter().map(|adjs| Degree::of_usize(adjs.len())).collect();
    
        for i in n.iter_verts() {
            for j in adj_list[i].iter() {
                adj[i][*j] = true;
            }
        }
        
        Graph {
            n,
            adj,
            adj_list,
            deg,
            constructor
        }
    }

    pub fn of_matrix(adj: VertexVec<VertexVec<bool>>, constructor: Constructor) -> Graph {
        let n = adj.len();
        let mut adj_list = VertexVec::new(n, &vec![]);
        let mut deg = VertexVec::new(n, &Degree::ZERO);

        for (i, j) in n.iter_pairs() {
            if adj[i][j] {
                adj_list[i].push(j);
                adj_list[j].push(i);
                deg[i].incr_inplace();
                deg[j].incr_inplace();
            }
        }

        Graph { 
            n,
            adj,
            adj_list,
            deg,
            constructor
        }
    }

    /**
     * For when we have a partial adjacency matrix, but it is not yet symmetric.
     * This constructs a symmetric version (adj | adj^T), and builds the graph from that.
     * e.g. used when undirecting a directed graph.
     */
    pub fn of_matrix_to_be_symmetrised(assym: &VertexVec<VertexVec<bool>>, constructor: Constructor) -> Graph {
        let n = assym.len();
        let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
        let mut adj_list = VertexVec::new(n, &vec![]);
        let mut deg = VertexVec::new(n, &Degree::ZERO);

        for (x, y) in n.iter_pairs() {
            if assym[x][y] || assym[y][x] {
                adj[x][y] = true;
                adj[y][x] = true;
                adj_list[x].push(y);
                adj_list[y].push(x);
                deg[x].incr_inplace();
                deg[y].incr_inplace();
            }
        }

        Graph { 
            n,
            adj,
            adj_list,
            deg,
            constructor
        }
    }

    pub fn new_complete(n: Order) -> Graph {
        let mut adj_list = VertexVec::new(n, &vec![]);

        for (i, j) in n.iter_pairs() {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }

        Graph::of_adj_list(adj_list, Raw(Complete(n)))
    }

    pub fn new_complete_bipartite(left: Order, right: Order) -> Graph {
        let mut adj_list = VertexVec::new(left + right, &vec![]);

        for j in right.iter_verts() {
            let k = j.incr_by_order(left);
            for i in left.iter_verts() {
                adj_list[i].push(k);
                adj_list[k].push(i);
            }
        }

        Graph::of_adj_list(adj_list, Raw(CompleteBipartite(left, right)))
    }

    pub fn new_complete_multipartite(orders: &Vec<Order>) -> Graph {
        let mut n_u = 0;
        for order in orders.iter() {
            n_u += order.to_usize();
        }
        let n = Order::of_usize(n_u);
        let mut adj_list = VertexVec::new(n, &vec![]);

        let mut num_vertices_placed = 0;
        for part in orders.iter() {
            for j in part.iter_verts() {
                let u = j.incr_by(num_vertices_placed);
                for i in 0..num_vertices_placed {
                    let v = Vertex::of_usize(i);
                    adj_list[u].push(v);
                    adj_list[v].push(u);
                }
            }
            num_vertices_placed += part.to_usize()
        }

        Graph::of_adj_list(adj_list, Raw(CompleteMultipartite(orders.to_owned())))
    }

    pub fn new_turan(n: Order, chi: usize) -> Graph {
        let mut adj_list = VertexVec::new(n, &vec![]);
        let mut num_vertices_placed = 0;
        let n_u = n.to_usize();

        for i in 1..=chi {
            let class_size = (i * n_u) / chi - num_vertices_placed;
            for u in 0..num_vertices_placed {
                for dv in 0..class_size {
                    let v = Vertex::of_usize(num_vertices_placed + dv);
                    let u_v = Vertex::of_usize(u);
                    adj_list[u_v].push(v);
                    adj_list[v].push(u_v);
                }
            }

            num_vertices_placed += class_size;
        }

        Graph::of_adj_list(adj_list, Raw(Turan(n, chi)))
    }

    pub fn new_cyclic(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for i in n.iter_verts() {
            adj_list[i].push(i.incr_wrap(n));
            adj_list[i].push(i.decr_wrap(n));
        }

        Graph::of_adj_list(adj_list, Raw(Cyclic(n)))
    }

    pub fn new_path(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for i in n.iter_verts().take(n.to_usize() - 1) {
            adj_list[i].push(i.incr());
        }
        for i in n.iter_verts().skip(1) {
            adj_list[i].push(i.decr());
        }

        Graph::of_adj_list(adj_list, Raw(Path(n)))
    }

    pub fn new_star(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);
        let zero = Vertex::of_usize(0);

        for i in n.iter_verts().skip(1) {
            adj_list[zero].push(i);
            adj_list[i].push(zero);
        }

        Graph::of_adj_list(adj_list, Raw(Star(n)))
    }

    pub fn new_empty(n: Order) -> Graph {
        let adj_list = VertexVec::new_fn(n, |_| vec![]);
        
        Graph::of_adj_list(adj_list, Raw(Empty(n)))
    }

    /**
     * Returns a new graph containing only those vertices for which the filter is true.
     */
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

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        (0..self.n.to_usize()).map(Vertex::of_usize)
    }

    pub fn iter_pairs(&self) -> impl Iterator<Item = (Vertex, Vertex)> {
        self.n.iter_pairs()
    }

    pub fn iter_edges(&self) -> EdgeIterator {
        EdgeIterator::new(self)
    }

    pub fn is_isomorphic_to(&self, g: &Graph) -> bool {
        isomorphisms::is_isomorphic_to(self, g)
    }

    /**
     * Only consider vertices for which filter[v] == true
     */
    pub fn filtered_components(&self, filter: Option<&VertexVec<bool>>) -> VertexVec<Component> {
        let filter = filter.map(BigVertexSet::of_filter);
        flood_fill::filtered_components(self, filter)
    }

    /**
     * Only consider edges in the given edge set.
     */
    pub fn edge_subset_components(&self, edges: EdgeSet, indexer: &EdgeIndexer) -> VertexVec<Component> {
        flood_fill::edge_subset_components(self, edges, indexer)
    }

    /**
     * Only consider vertices in the given vertex set.
     */
    pub fn vertex_subset_components(&self, vs: BigVertexSet) -> VertexVec<Component> {
        flood_fill::filtered_components(self, Some(vs))
    }
    
    pub fn components(&self) -> VertexVec<Component> {
        self.filtered_components(None)
    }
    
    /**
     * Returns the size of the largest component
     */
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

    /**
     * Returns an arbitrary vertex at least min_dist away from start, or None if no such
     * vertex exists.
     */
    pub fn get_vertex_far_from(&self, start: Vertex, min_dist: u32) -> Option<Vertex> {
        let dists = self.flood_fill_dist(start);
        for (v, dist) in dists.iter_enum() {
            if dist.map_or(false, |x| x >= min_dist) {
                return Some(v)
            }
        }
        None
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
        flood_fill::flood_fill(self, start, end, filter)
    }
    
    pub fn flood_fill_dist(&self, start: Vertex) -> VertexVec<Option<u32>> {
        flood_fill::flood_fill_dist(self, start)
    }
    
    pub fn flood_fill_two_colourable(&self, edge_filter: &EdgeSet, indexer: &EdgeIndexer) -> bool {
        flood_fill::flood_fill_two_colourable(self, edge_filter, indexer)
    }
    
    pub fn flood_fill_edge_components(&self, edge_filter: &EdgeSet, indexer: &EdgeIndexer) -> EdgeVec<Option<Component>> {
        flood_fill::flood_fill_edge_components(self, edge_filter, indexer)
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
        println!("degs: {:?}", self.deg.iter().map(|x| x.to_usize()).collect::<Vec<usize>>());
        println!("{}", self.n);
        for i in self.n.iter_verts() {
            if self.deg[i].at_least(1) {
                print!("{} ~ ", i);
                print!("{}", self.adj_list[i][0]);
                for j in self.adj_list[i].iter().skip(1) {
                    print!(", {}", j);
                }
                println!();
            }
        }
    }

    pub fn print_matrix(&self, should_print_commas: bool, should_print_newlines: bool) {
        println!("n: {}, m: {}", self.n, self.size());
        for i in self.n.iter_verts() {
            for j in self.n.iter_verts() {
                if self.adj[i][j] {
                    print!("1");
                } else {
                    print!("0");
                }
                if should_print_commas {
                    print!(", ");
                }
            }
            if should_print_newlines {
                println!()
            }
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

    pub fn min_degree(&self) -> Degree {
        *self.deg.iter().min().unwrap()
    }

    pub fn max_degree(&self) -> Degree {
        *self.deg.iter().max().unwrap()
    }

    pub fn max_codegree(&self) -> usize {
        let mut out = 0;
        let nbhds = self.get_open_nbhds();
        for (u, v) in self.iter_pairs() {
            let codeg = nbhds[u].inter(&nbhds[v]).size();
            if codeg > out {
                out = codeg
            }
        }
        out
    }

    pub fn degree_sequence(&self) -> &Vec<Degree> {
        self.deg.drop_vertices()
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

    pub fn diameter(&self) -> u32 {
        let dist = self.floyd_warshall();
        let mut diam = 0;
        for (u, v) in self.n.iter_pairs() {
            if dist[u][v] > diam {
                diam = dist[u][v];
            }
        }
        diam as u32
    }

    pub fn radius(&self) -> u32 {
        let dist = self.floyd_warshall();
        let mut radius = usize::MAX;
        for u in self.iter_verts() {
            let mut furthest = 0;
            for v in self.iter_verts() {
                if dist[u][v] > furthest {
                    furthest = dist[u][v];
                }
            }
            if furthest < radius {
                radius = furthest;
            }
        }
        radius as u32
    }

    pub fn min_distance_sum(&self, targets: VertexSet) -> usize {
        let dist = self.floyd_warshall();
        let mut min_sum = usize::MAX;
        for u in self.iter_verts() {
            let mut sum = 0;
            for v in targets.iter() {
                sum += dist[u][v];
            }
            if sum < min_sum {
                min_sum = sum;
            }
        }
        min_sum
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

    /**
     * Returns the open nbhd of v (does not include v)
     */
    pub fn open_nbhd(&self, v: Vertex) -> VertexSet {
        let mut nbhd = VertexSet::new(self.n);
        for u in self.adj_list[v].iter() {
            nbhd.add_vert(*u);
        }
        nbhd
    }

    /**
     * Returns the closed nbhd of v (does include v)
     */
    pub fn closed_nbhd(&self, v: Vertex) -> VertexSet {
        self.open_nbhd(v).add_vert_immutable(v)
    }

    /**
     * Returns a VertexVec of the open nbhds of each point
     * open nbhds do not include the point itself
     */
    pub fn get_open_nbhds(&self) -> VertexVec<VertexSet> {
        let mut nbhds = VertexVec::new(self.n, &VertexSet::new(self.n));
        for (v, nbhd) in nbhds.iter_mut_enum() {
            *nbhd = self.open_nbhd(v)
        }
        nbhds
    }

    /**
     * Returns a VertexVec of the closed nbhds of each point
     * open nbhds do include the point itself
     */
    pub fn get_closed_nbhds(&self) -> VertexVec<VertexSet> {
        let mut nbhds = VertexVec::new(self.n, &VertexSet::new(self.n));
        for (v, nbhd) in nbhds.iter_mut_enum() {
            *nbhd = self.closed_nbhd(v)
        }
        nbhds
    }

    /** 
     * Returns the bfs width of the graph; that is, the maximum queue size
     * during a BFS
     */
    pub fn get_bfs_width(&self) -> usize {
        let mut bfs_iterator = self.iter_verts_bfs();
        while let Some(_) = bfs_iterator.next() { }
        bfs_iterator.max_width
    }

    pub fn iter_vertex_subsets(&self) -> VertexSubsetIterator {
        VertexSubsetIterator::new(self.n)
    }

    pub fn iter_edge_sets(&self) -> AllEdgeSetsIterator {
        AllEdgeSetsIterator::new(&self.adj_list)
    }

    pub fn iter_verts_bfs(&self) -> BFSIterator<'_> {
        BFSIterator::new(self)
    }

    #[allow(dead_code)]
    pub fn test_graph(index: usize) -> Graph {
        crate::constructor::from_file::new_entity(&format!("test/test_{}", index)).as_owned_graph()
    }

    pub fn serialise(&self) -> String {
        let mut out = String::new();
        for v in self.iter_verts().take(self.n.to_usize() - 1) {
            for u in self.adj_list[v].iter() {
                if *u > v {
                    out.push_str(&format!("*{}", u))
                }
            }
            out.push_str("|")
        }
        out
    }

    pub fn deserialise(code: &str) -> Graph {
        let pars = code.split("|").collect::<Vec<&str>>();
        let n = Order::of_usize(pars.len());
        let mut adj_list = VertexVec::new(n, &vec![]);
        for (i, par) in pars.iter().enumerate() {
            let u = Vertex::of_usize(i);
            for v in par.split("*").skip(1) {
                let v = Vertex::of_string(v);
                adj_list[u].push(v);
                adj_list[v].push(u);
            }
        }
        Graph::of_adj_list(adj_list, Serialised(code.to_owned()))
    }
}

pub fn adj_list_of_manual(adj_list: Vec<Vec<usize>>) -> VertexVec<Vec<Vertex>> {
    adj_list.iter()
        .map(|l| l.iter()
            .map(|x| Vertex::of_usize(*x))
            .collect())
        .collect()
}

pub struct BFSIterator<'a> {
    q: Queue<Vertex>,
    visited: VertexVec<bool>,
    next_vert: Vertex,
    g: &'a Graph,
    max_width: usize,
}

impl BFSIterator<'_> {
    pub fn new(g: &Graph) -> BFSIterator<'_> {
        BFSIterator {
            q: Queue::new(),
            visited: VertexVec::new(g.n, &false),
            next_vert: Vertex::ZERO,
            g,
            max_width: 0
        }
    }
}

impl Iterator for BFSIterator<'_> {
    type Item = Vertex;

    fn next(&mut self) -> Option<Self::Item> {
        fn process_v(iterator: &mut BFSIterator, v: Vertex) -> Option<Vertex> {
            for u in iterator.g.adj_list[v].iter() {
                if !iterator.visited[*u] {
                    iterator.visited[*u] = true;
                    let _ = iterator.q.add(*u);
                }
            }
            iterator.max_width = iterator.max_width.max(iterator.q.size());
            Some(v)
        }
        if let Ok(v) = self.q.remove() {
            process_v(self, v)
        } else {
            while !self.next_vert.is_n(self.g.n) && self.visited[self.next_vert] {
                self.next_vert.incr_inplace()
            }
            if self.next_vert.is_n(self.g.n) {
                None
            } else {
                self.visited[self.next_vert] = true;
                process_v(self, self.next_vert)
            }
        }
    }
}

impl EdgeIterator<'_> {
    fn new(g: &Graph) -> EdgeIterator<'_> {
        EdgeIterator {
            g,
            sub_iter: VertexPairIterator::new(g.n),
        }
    }
}

impl Iterator for EdgeIterator<'_> {
    type Item = Edge;

    fn next(&mut self) -> Option<Self::Item> {
        let mut pair = self.sub_iter.next();
        let mut e = None;
        loop {
            if let Some((x, y)) = pair {
                if self.g.adj[x][y] {
                    e = Some(Edge::of_pair(x, y));
                    break;
                }
            } else {
                break;
            }
            pair = self.sub_iter.next();
        }
        e
    }
}