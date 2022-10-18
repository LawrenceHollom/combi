mod domination;
mod chromatic;
mod max_acyclic;
mod bunkbed;
mod percolate;
mod bunkbed_posts;

use std::collections::HashMap;

use crate::instruction::*;
use crate::graph::*;

pub struct Operator {
    previous_values: HashMap<IntOperation, u32>
}

impl Operator {
    fn operate_int(&mut self, g: &Graph, operation: &IntOperation) -> u32 {
        match self.previous_values.get(operation) {
            Some(value) => *value,
            None => {
                let value = match operation {
                    IntOperation::DominationNumber => domination::domination_number(g),
                    IntOperation::ChromaticNumber => chromatic::chromatic_number(g),
                    IntOperation::MaxAcyclicSubgraph => max_acyclic::max_acyclic_subgraph(g),
                    IntOperation::CliqueCoveringNumber => chromatic::chromatic_number(&g.complement()),
                };
                self.previous_values.insert(*operation, value);
                value
            }
        }
    }

    fn operate_bool(&mut self, g: &Graph, operation: &BoolOperation) -> bool {
        match operation {
            BoolOperation::Less(op1, op2) => 
                self.operate_int(g, op1) < self.operate_int(g, op2),
            BoolOperation::More(op1, op2) => 
                self.operate_int(g, op1) > self.operate_int(g, op2),
            BoolOperation::NotLess(op1, op2) => 
                self.operate_int(g, op1) >= self.operate_int(g, op2),
            BoolOperation::NotMore(op1, op2) => 
                self.operate_int(g, op1) <= self.operate_int(g, op2),
        }
    }

    fn operate_unit(&mut self, g: &Graph, operation: &UnitOperation) {
        match operation {
            UnitOperation::Print => g.print(),
            UnitOperation::RawBunkbed => bunkbed::print_polynomials(g),
            UnitOperation::BunkbedPosts => bunkbed_posts::print_polynomials(g),
        }
    }

    pub fn operate(&mut self, g: &Graph, operation: &Operation) -> String {
        match operation {
            Operation::Int(op) => 
                u32::to_string(&self.operate_int(g, op)),
            Operation::Bool(op) =>
                bool::to_string(&self.operate_bool(g, op)),
            Operation::Unit(op) => {
                self.operate_unit(g, op);
                "()".to_owned()
            }
        }
    }

    pub fn new() -> Operator {
        Operator { previous_values: HashMap::new() }
    }
}