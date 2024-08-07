use utilities::*;

use digraph::Digraph;

pub mod digraph;
pub mod graph;
pub mod poset;

use graph::*;
use poset::*;

#[derive(Clone)]
pub enum Entity {
    Graph(graph::Graph),
    Poset(poset::Poset),
    Digraph(digraph::Digraph),
}

impl Entity {
    pub fn as_graph(&self) -> &Graph {
        use Entity::*;
        match self {
            Graph(g) => g,
            Poset(_) => panic!("Cannot convert poset to graph!"),
            Digraph(_) => panic!("Cannot convert digraph to graph!"),
        }
    }

    pub fn as_graph_opn(&self) -> Option<&Graph> {
        use Entity::*;
        match self {
            Graph(g) => Some(g),
            Poset(_) => None,
            Digraph(_) => None,
        }
    }

    pub fn as_owned_graph(self) -> Graph {
        use Entity::*;
        match self {
            Graph(g) => g,
            Poset(_) => panic!("Cannot convert poset to graph!"),
            Digraph(_) => panic!("Cannot convert digraph to graph!"),
        }
    }

    pub fn as_digraph(&self) -> &Digraph {
        use Entity::*;
        match self {
            Graph(_) => panic!("This is the one thing we didn't want to happen (Graph -> Digraph as reference)"),
            Digraph(d) => d,
            Poset(_) => panic!("Cannot convert poset to digraph!"),
        }
    }

    pub fn as_owned_digraph(self) -> Digraph {
        use Entity::*;
        match self {
            Graph(g) => digraph::Digraph::of_matrix(g.adj),
            Digraph(d) => d,
            Poset(_) => panic!("Cannot convert poset to digraph!"),
        }
    }

    pub fn as_poset(&self) -> &Poset {
        use Entity::*;
        match self {
            Poset(p) => p,
            Graph(_) => panic!("Cannot convert graph to poset!"),
            Digraph(_) => panic!("Cannot convert digraph to poset!"),
        }
    }

    pub fn print(&self) {
        use Entity::*;
        match self {
            Poset(p) => p.print(),
            Graph(g) => g.print(),
            Digraph(d) => d.print(),
        }
    }

    pub fn order(&self) -> Order {
        use Entity::*;
        match self {
            Poset(p) => p.order,
            Graph(g) => g.n,
            Digraph(d) => d.n,
        }
    }

    pub fn is_connected(&self) -> bool {
        use Entity::*;
        match self {
            Poset(p) => p.is_connected(),
            Graph(g) => g.is_connected(),
            Digraph(d) => d.is_connected(),
        }
    }
}