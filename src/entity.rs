pub mod digraph;
pub mod graph;
pub mod poset;

pub enum Entity {
    Graph(graph::Graph),
    Poset(poset::Poset),
    Digraph(digraph::Digraph),
}