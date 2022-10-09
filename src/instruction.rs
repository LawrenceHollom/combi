extern crate utilities;

use utilities::*;
use std::fmt;

pub enum Constructor {
    RandomRegularBipartite(Order, Degree),
    Complete(Order),
}

pub enum Operation {
    DominationNumber,
    ChromaticNumber,
    MaxAcyclicSubgraph,
    CliqueCoveringNumber,
}

pub struct Instruction {
    pub constructor: Constructor,
    pub operation: Operation,
}

impl Constructor {
    pub fn of_string(text: &str) -> Constructor {
        // must be otf func_tion(a, b, c, ...)
        let pars: Vec<&str> = text.split('(').collect();
        let func: &str = pars[0];
        let args: Vec<&str> = pars[1].split(',').map(|par| par.trim().trim_matches(')')).collect();
        match func {
            "rrb" | "random_regular_bipartite" => 
                Constructor::RandomRegularBipartite(Order::of_string(args[0]), Degree::of_string(args[1])),
            "complete" | "K" => 
                Constructor::Complete(Order::of_string(args[0])),
            &_ => panic!(),
        }
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constructor::RandomRegularBipartite(order, deg) => {
                write!(f, "Random regular bipartite or order {} and degree {}", order, deg)
            },
            Constructor::Complete(order) => {
                write!(f, "Complete of order {}", order)
            },
        }
    }
}

impl Operation {
    pub fn of_string(text: &str) -> Operation {
        match text.trim() {
            "domination" | "dominator" | "gamma" => Operation::DominationNumber,
            "chromatic" | "chi" => Operation::ChromaticNumber,
            "max_acyclic" | "acyclic" => Operation::MaxAcyclicSubgraph,
            "clique_cover" | "theta" => Operation::CliqueCoveringNumber,
            &_ => panic!(),
        }
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            Operation::DominationNumber => "Domination number",
            Operation::ChromaticNumber => "Chromatic number",
            Operation::MaxAcyclicSubgraph => "Max acyclic subgraph size",
            Operation::CliqueCoveringNumber => "Clique covering number",
        };
        write!(f, "{}", name)
    }
}

impl Instruction {
    pub fn of_string(text: &str) -> Instruction {
        let pars: Vec<&str> = text.split("->").map(|par| par.trim()).collect();
        let constructor = Constructor::of_string(pars[0]);
        let operation = Operation::of_string(pars[1]);
        Instruction {
            constructor,
            operation,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Constructor: {}\nOperation: {}", self.constructor, self.operation)
    }
}