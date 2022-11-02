mod domination;
mod chromatic;
mod max_acyclic;
mod components;
mod monotone;
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
                    IntOperation::Order => g.n.to_usize() as u32,
                    IntOperation::Size => g.size() as u32,
                    IntOperation::LargestComponent => components::largest_component(g),
                    IntOperation::NumComponents => components::num_components(g),
                    IntOperation::DominationNumber => domination::domination_number(g),
                    IntOperation::ChromaticNumber => chromatic::chromatic_number(g),
                    IntOperation::MaxAcyclicSubgraph => max_acyclic::max_acyclic_subgraph(g),
                    IntOperation::CliqueCoveringNumber => chromatic::chromatic_number(&g.complement()),
                    IntOperation::NumOnLongMonotone => monotone::num_on_long_monotone(&g),
                };
                self.previous_values.insert(*operation, value);
                value
            }
        }
    }

    pub fn operate_bool(&mut self, g: &Graph, operation: &BoolOperation) -> bool {
        match operation {
            BoolOperation::Less(op1, op2) => 
                self.operate_int(g, op1) < self.operate_int(g, op2),
            BoolOperation::More(op1, op2) => 
                self.operate_int(g, op1) > self.operate_int(g, op2),
            BoolOperation::NotLess(op1, op2) => 
                self.operate_int(g, op1) >= self.operate_int(g, op2),
            BoolOperation::NotMore(op1, op2) => 
                self.operate_int(g, op1) <= self.operate_int(g, op2),
            BoolOperation::IsConnected => components::is_connected(g),
            BoolOperation::HasLongMonotone => monotone::has_long_monotone(g),
        }
    }

    pub fn operate_float(&mut self, g: &Graph, operation: &FloatOperation) -> f64 {
        match operation {
            FloatOperation::OfBool(operation) => 
                if self.operate_bool(g, operation) { 1.0 } else { 0.0 },
            FloatOperation::OfInt(operation) =>
                self.operate_int(g, operation) as f64,
            FloatOperation::Ratio(op1, op2) =>
                (self.operate_int(g, op1) as f64) / (self.operate_int(g, op2) as f64),
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
            Operation::Float(op) =>
                format!("{:.4}", self.operate_float(g, op)),
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