use std::fmt::*;
use std::ops::Index;

use queues::*;

use utilities::*;
use utilities::vertex_tools::*;
use utilities::component_tools::*;
use utilities::edge_tools::*;

use super::graph_and_metadata::*;

#[derive(Clone)]
pub struct EquivalenceRelation {
	pub n: Order,
	components: VertexVec<Component>,
	vertices: VertexSet,
}

/**
 * config has an edge if that edge is down.
 * Returns a VertexVec of items otf [is down-connected to 0, is up-connected to 0]
 */
pub fn get_connections(g_etc: &GraphAndMetadata, u: Vertex, config: &EdgeSet, indexer: &EdgeIndexer) -> VertexVec<[bool; 2]> {
    let mut q = queue![(u, 0)];
    let mut visited = VertexVec::new(g_etc.get_n(), &[false, false]);
	visited[u][0] = true;
	while let Ok((v, layer)) = q.remove() {
		if g_etc.has_post(v) && !visited[v][1 - layer] {
			visited[v][1 - layer] = true;
			let _ = q.add((v, 1 - layer));
		}
		for u in g_etc.iter_adj_list(v) {
			if !visited[*u][layer] && config.has_edge(Edge::of_pair(v, *u), indexer) == (layer == 0) {
				visited[*u][layer] = true;
				let _ = q.add((*u, layer));
			}
		}
	}
	visited
}


impl EquivalenceRelation {
	pub fn new(g_etc: &GraphAndMetadata, config: &EdgeSet, indexer: &EdgeIndexer) -> EquivalenceRelation {
		fn pack_vertex(n: Order, x: Vertex, level: bool) -> Vertex {
			if level {
				x.incr_by_order(n)
			} else {
				x
			}
		}
		let n = g_etc.get_n();
		let mut union = UnionFind::new(n.times(2));
		for v in g_etc.iter_targets() {
			let down_conns = get_connections(&g_etc, v, config, indexer);
			let up_conns = get_connections(&g_etc, v, &config.inverse(indexer), indexer);
			for u in g_etc.iter_targets() {
				if down_conns[u][0] {
					union.merge(pack_vertex(n, u, false), pack_vertex(n, v, false))
				}
				if down_conns[u][1] {
					union.merge(pack_vertex(n, u, true), pack_vertex(n, v, false))
				}
				if up_conns[u][0] {
					union.merge(pack_vertex(n, u, true), pack_vertex(n, v, true))
				}
				if up_conns[u][1] {
					union.merge(pack_vertex(n, u, false), pack_vertex(n, v, true))
				}
			}
		}
		EquivalenceRelation { 
			n,
			components : union.to_canonical_component_vec(),
			vertices: g_etc.get_targets()
		 }
	}

    pub fn get_k(&self) -> usize {
        return self.vertices.size()
    }

    pub fn iter_vertices(&self) -> impl Iterator<Item = Vertex> {
        self.vertices.iter()
    }
}

impl Debug for EquivalenceRelation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		let _ = write!(f, "[ ");
		for v in self.vertices.iter() {
			let _ = write!(f, "{} ", self.components[v].to_vertex().to_string());
		}
		let _ = write!(f, "/ ");
		for v in self.vertices.iter() {
			let _ = write!(f, "{} ", self.components[v.incr_by_order(self.n)].to_vertex().to_string());
		}
        write!(f, "]")
    }
}

impl Index<Vertex> for EquivalenceRelation {
    type Output = Component;

    fn index(&self, index: Vertex) -> &Self::Output {
        &self.components[index]
    }
}