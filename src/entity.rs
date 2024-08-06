mod digraph;
mod graph;
mod poset;

pub enum Entity {
    Graph(graph::Graph),
    Poset(poset::Poset),
    Digraph(digraph::Digraph),
}