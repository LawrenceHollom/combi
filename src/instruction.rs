extern crate utilities;

use utilities::*;

pub enum Constructor {
    RandomRegularBipartite(Order, Degree),
    Complete(Order),
}

pub enum Operation {
    DominationNumber,
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

    pub fn to_string(&self) -> &str {
        match self {
            Constructor::RandomRegularBipartite(_order, _deg) => {
                "Random regular bipartite"
            },
            Constructor::Complete(_order) => {
                "Complete"
            },
        }
    }
}

impl Operation {
    pub fn of_string(text: &str) -> Operation {
        match text.trim() {
            "domination" => Operation::DominationNumber,
            &_ => panic!(),
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Operation::DominationNumber => "Domination number",
        }
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

    pub fn to_string(&self) -> String {
        "Constructor: ".to_owned() + self.constructor.to_string() 
                + "\nOperation: " + self.operation.to_string()
    }
}