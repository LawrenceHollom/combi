mod domination;
mod chromatic;
mod max_acyclic;
mod components;
mod monotone;
mod bunkbed;
mod percolate;
mod bunkbed_posts;
mod cliques;
mod girth;

use std::collections::HashMap;

use crate::operation::*;
use crate::graph::*;

use crate::operation::bool_operation::*;
use crate::operation::float_operation::*;
use crate::operation::int_operation::*;
use crate::operation::unit_operation::*;

pub struct Operator {
    previous_values: HashMap<IntOperation, u32>
}

impl Operator {
    fn operate_int(&mut self, g: &Graph, operation: &IntOperation) -> u32 {
        use IntOperation::*;
        match self.previous_values.get(operation) {
            Some(value) => *value,
            None => {
                let value = match operation {
                    Order => g.n.to_usize() as u32,
                    Size => g.size() as u32,
                    LargestComponent => components::largest_component(g),
                    NumComponents => components::num_components(g),
                    CliqueNumber => cliques::largest_clique(g),
                    IndependenceNumber => cliques::independence_number(g),
                    Girth => girth::girth(g),
                    DominationNumber => domination::domination_number(g),
                    ChromaticNumber => chromatic::chromatic_number(g),
                    MaxAcyclicSubgraph => max_acyclic::max_acyclic_subgraph(g),
                    CliqueCoveringNumber => chromatic::chromatic_number(&g.complement()),
                    NumOnLongMonotone => monotone::num_on_long_monotone(&g),
                    MaxMonotonePath => monotone::max_monotone(&g),
                    TotalDominationGameLength => domination::total_domination_game_length(&g),
                    Number(k) => *k,
                };
                self.previous_values.insert(*operation, value);
                value
            }
        }
    }
    
    fn operate_int_to_bool_infix(&mut self, g: &Graph, infix: &IntToBoolInfix, op1: &IntOperation, op2: &IntOperation) -> bool {
        use IntToBoolInfix::*;
        let x1 = self.operate_int(g, op1);
        let x2 = self.operate_int(g, op2);
        match *infix {
            Less => x1 < x2,
            More => x1 > x2,
            NotLess => x1 >= x2,
            NotMore => x1 >= x2,
            Equal => x1 == x2,
        }
    }
    
    fn operate_bool_to_bool_infix(&mut self, g: &Graph, infix: &BoolToBoolInfix, op1: &BoolOperation, op2: &BoolOperation) -> bool {
        use BoolToBoolInfix::*;
        let x1 = self.operate_bool(g, op1);
        let x2 = self.operate_bool(g, op2);
        match *infix {
            And => x1 && x2,
            Or => x1 || x2,
        }
    }

    pub fn operate_bool(&mut self, g: &Graph, operation: &BoolOperation) -> bool {
        
        use BoolOperation::*;
        match operation {
            IntInfix(infix, op1, op2) => 
                self.operate_int_to_bool_infix(g, infix, op1, op2),
            BoolInfix(infix, op1, op2) => 
                self.operate_bool_to_bool_infix(g, infix, op1, op2),
            IsConnected => components::is_connected(g),
            HasLongMonotone => monotone::has_long_monotone(g),
        }
    }

    pub fn operate_float(&mut self, g: &Graph, operation: &FloatOperation) -> f64 {
        use FloatOperation::*;
        match operation {
            OfBool(operation) => 
                if self.operate_bool(g, operation) { 1.0 } else { 0.0 },
            OfInt(operation) =>
                self.operate_int(g, operation) as f64,
            Ratio(op1, op2) =>
                (self.operate_int(g, op1) as f64) / (self.operate_int(g, op2) as f64),
        }
    }

    fn operate_unit(&mut self, g: &Graph, operation: &UnitOperation) {
        use UnitOperation::*;
        match operation {
            Print => g.print(),
            RawBunkbed => bunkbed::print_polynomials(g),
            BunkbedPosts => bunkbed_posts::print_polynomials(g),
            BunkbedSimulation => bunkbed::simulate(g),
            Unit => (),
        }
    }

    pub fn operate(&mut self, g: &Graph, operation: &Operation) -> String {
        use Operation::*;
        match operation {
            Int(op) => u32::to_string(&self.operate_int(g, op)),
            Bool(op) => bool::to_string(&self.operate_bool(g, op)),
            Float(op) => format!("{:.4}", self.operate_float(g, op)),
            Unit(op) => {
                self.operate_unit(g, op);
                "()".to_owned()
            }
        }
    }

    pub fn new() -> Operator {
        Operator { previous_values: HashMap::new() }
    }
}