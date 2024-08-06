use utilities::vertex_tools::*;
use crate::entity::graph::*;

// Test components, but only considering vertices in the filter.
pub fn filtered_components(g: &Graph, filter: Option<&VertexVec<bool>>) -> VertexVec<Component> {
    let mut comp: VertexVec<Option<Vertex>> = VertexVec::new(g.n, &None);
    let mut q: Queue<Vertex> = queue![];

    for i in g.n.iter_verts() {
        if comp[i].is_none() {
            comp[i] = Some(i);
            if filter.map_or(true, |f| f[i]) {
                let _ = q.add(i);
                'flood_fill: loop {
                    match q.remove() {
                        Ok(node) => {
                            for j in g.adj_list[node].iter() {
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

pub fn flood_fill(g: &Graph, start: Vertex, end: Option<Vertex>, filter: Option<&VertexVec<bool>>) -> VertexVec<Option<Vertex>> {
    let mut prev = VertexVec::new(g.n, &None);
    let mut q: Queue<Vertex> = queue![];
    prev[start] = Some(start);
    let _ = q.add(start);
    'flood_fill: loop {
        match q.remove() {
            Ok(node) => {
                for next in g.adj_list[node].iter() {
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
                g.print();
                panic!("AAAAAAAAAA {}", e)
            }
        }
    }
    prev
}

pub fn flood_fill_dist(g: &Graph, start: Vertex) -> VertexVec<Option<u32>> {
    let mut connected = VertexVec::new(g.n, &None);
    let mut q: Queue<Vertex> = queue![];
    connected[start] = Some(0);
    let _ = q.add(start);
    while q.size() > 0 {
        let node = q.remove().unwrap();
        for v in g.adj_list[node].iter() {
            if connected[*v].is_none() {
                connected[*v] = connected[node].map(|x| x + 1);
                let _ = q.add(*v);
            }
        }
    }
    connected
}

pub fn flood_fill_two_colourable(g: &Graph, edge_filter: &EdgeSet, indexer: &EdgeIndexer) -> bool {
    let mut colour = VertexVec::new(g.n, &None);
    let mut q: Queue<Vertex> = queue![];
    let mut is_two_colourable = true;
    'test_verts: for v in g.n.iter_verts() {
        if colour[v].is_none() {
            colour[v] = Some(0);
            let _ = q.add(v);
            while q.size() > 0 {
                let u = q.remove().unwrap();
                for w in g.adj_list[u].iter() {
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

pub fn flood_fill_edge_components(g: &Graph, edge_filter: &EdgeSet, indexer: &EdgeIndexer) -> EdgeVec<Option<Component>> {
    let mut components = EdgeVec::new(&g.adj_list, None);
    let mut q: Queue<Vertex> = queue![];
    let mut visited: VertexVec<bool> = VertexVec::new(g.n, &false);
    for v in g.n.iter_verts() {
        if !visited[v] {
            visited[v] = true;
            let _ = q.add(v);
            let comp = Component::of_vertex(v);
            while q.size() > 0 {
                let u = q.remove().unwrap();
                for w in g.adj_list[u].iter() {
                    let e = Edge::of_pair(u, *w);
                    if edge_filter.has_edge(e, indexer) {
                        components[e] = Some(comp);
                        if !visited[*w] {
                            let _ = q.add(*w);
                            visited[*w] = true;
                        }
                    }
                }
            }
        }
    }
    components
}