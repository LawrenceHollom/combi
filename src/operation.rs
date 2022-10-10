mod domination;
mod chromatic;
mod max_acyclic;

use crate::instruction::*;
use crate::graph::*;

fn operate_int(g: &Graph, operation: &IntOperation) -> u32 {
    match operation {
        IntOperation::DominationNumber => domination::domination_number(g),
        IntOperation::ChromaticNumber => chromatic::chromatic_number(g),
        IntOperation::MaxAcyclicSubgraph => max_acyclic::max_acyclic_subgraph(g),
        IntOperation::CliqueCoveringNumber => chromatic::chromatic_number(&g.complement()),
    }
}

fn operate_bool(g: &Graph, operation: &BoolOperation) -> bool {
    match operation {
        BoolOperation::Less(op1, op2) => 
            operate_int(g, op1) < operate_int(g, op2),
        BoolOperation::More(op1, op2) => 
            operate_int(g, op1) > operate_int(g, op2),
        BoolOperation::NotLess(op1, op2) => 
            operate_int(g, op1) >= operate_int(g, op2),
        BoolOperation::NotMore(op1, op2) => 
            operate_int(g, op1) <= operate_int(g, op2),
    }
}

fn operate_unit(g: &Graph, operation: &UnitOperation) {
    match operation {
        UnitOperation::Print => g.print(),
    }
}

pub fn operate(g: &Graph, operation: &Operation) -> String {

    match operation {
        Operation::Int(op) => 
            u32::to_string(&operate_int(g, op)),
        Operation::Bool(op) =>
            bool::to_string(&operate_bool(g, op)),
        Operation::Unit(op) => {
            operate_unit(g, op);
            "()".to_owned()
        }
    }
}