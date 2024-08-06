use digraph::Digraph;

pub mod digraph;
pub mod graph;
pub mod poset;

use graph::*;
use poset::*;

pub enum Entity {
    Graph(graph::Graph),
    Poset(poset::Poset),
    Digraph(digraph::Digraph),
}

impl Entity {
    pub fn as_graph(self) -> Graph {
        use Entity::*;
        match self {
            Graph(g) => g,
            Poset(_) => panic!("Cannot convert poset to graph!"),
            Digraph(_) => panic!("Cannot convert digraph to graph!"),
        }
    }

    pub fn as_digraph(self) -> Digraph {
        use Entity::*;
        match self {
            Graph(g) => digraph::Digraph::of_matrix(g.adj),
            Digraph(d) => d,
            Poset(_) => panic!("Cannot convert poset to digraph!"),
        }
    }

    pub fn as_poset(self) -> Poset {
        use Entity::*;
        match self {
            Poset(p) => p,
            Graph(_) => panic!("Cannot convert graph to poset!"),
            Digraph(_) => panic!("Cannot convert digraph to poset!"),
        }
    }
}