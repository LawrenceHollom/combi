use utilities::{*, edge_tools::*, vertex_tools::*, component_tools::*};
use crate::constructor::*;
use Constructor::*;
use RawConstructor::*;

use rand::{thread_rng};
use queues::*;

mod flood_fill;
mod isomorphisms;
mod cycles;

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

    pub fn new_complete(n: Order) -> Graph {
        let mut adj_list = VertexVec::new_fn(n, |_| vec![]);

        for (i, j) in n.iter_pairs() {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }

        Graph::of_adj_list(adj_list, Raw(Complete(n)))
    }

    pub fn new_complete_bipartite(left: Order, right: Order) -> Graph {
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

    pub fn is_isomorphic_to(&self, g: &Graph) -> bool {
        isomorphisms::is_isomorphic_to(self, g)
    }

    pub fn filtered_components(&self, filter: Option<&VertexVec<bool>>) -> VertexVec<Component> {
        flood_fill::filtered_components(self, filter)
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

    pub fn remove_all_k_cycles(&mut self, k: usize) {    
        cycles::remove_all_k_cycles(self, k)
    }

    pub fn iter_edge_sets(&self) -> EdgeSetIterator {
        EdgeSetIterator::new(&self.adj_list)
    }

    #[allow(dead_code)]
    pub fn test_graph(index: usize) -> Graph {
        crate::constructor::from_file::new_graph(&format!("test_{}", index))
    }
}

pub fn adj_list_of_manual(adj_list: Vec<Vec<usize>>) -> VertexVec<Vec<Vertex>> {
    adj_list.iter()
        .map(|l| l.iter()
            .map(|x| Vertex::of_usize(*x))
            .collect())
        .collect()
}