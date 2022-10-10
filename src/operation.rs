mod domination;
mod chromatic;
mod max_acyclic;

use crate::instruction::*;
use crate::graph::*;

pub fn operate(g: &Graph, operation: &Operation) -> u32 {
    match operation {
        Operation::DominationNumber => domination::domination_number(g),
        Operation::ChromaticNumber => chromatic::chromatic_number(g),
        Operation::MaxAcyclicSubgraph => max_acyclic::max_acyclic_subgraph(g),
        Operation::CliqueCoveringNumber => chromatic::chromatic_number(&g.complement()),
    }
}