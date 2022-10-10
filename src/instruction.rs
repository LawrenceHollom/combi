use utilities::*;
use std::fmt;
use regex;

pub enum Constructor {
    RandomRegularBipartite(Order, Degree),
    Complete(Order),
    FanoPlane,
    Petersen,
}

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
pub enum IntOperation {
    DominationNumber,
    ChromaticNumber,
    MaxAcyclicSubgraph,
    CliqueCoveringNumber,
}

pub enum BoolOperation {
    More(Box<IntOperation>, Box<IntOperation>),
    Less(Box<IntOperation>, Box<IntOperation>),
    NotMore(Box<IntOperation>, Box<IntOperation>),
    NotLess(Box<IntOperation>, Box<IntOperation>),
}

pub enum UnitOperation {
    Print,
}

pub enum Operation {
    Int(IntOperation),
    Bool(BoolOperation),
    Unit(UnitOperation),
}

pub struct Instruction {
    pub repeats: usize,
    pub constructor: Constructor,
    pub operations: Vec<Operation>,
}

impl Constructor {
    pub fn of_string(text: &str) -> Constructor {
        // must be otf func_tion(a, b, c, ...)
        let pars: Vec<&str> = text.split('(').collect();
        let func: &str = pars[0];
        
        let args: Vec<&str> = 
            if pars.len() > 1 {
                pars[1].split(',').map(|par| par.trim().trim_matches(')')).collect()
            } else {
                vec![]
            };
        
        match func {
            "rrb" | "random_regular_bipartite" => 
                Constructor::RandomRegularBipartite(Order::of_string(args[0]), Degree::of_string(args[1])),
            "complete" | "K" => Constructor::Complete(Order::of_string(args[0])),
            "fano" => Constructor::FanoPlane,
            "petersen" => Constructor::Petersen,
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
            Constructor::FanoPlane => write!(f, "the Fano plane"),
            Constructor::Petersen => write!(f, "the Petersen graph"),
        }
    }
}

impl IntOperation {
    pub fn of_string_result(text: &str) -> Option<IntOperation> {
        match text.trim().to_lowercase().as_str() {
            "domination" | "dominator" | "gamma" => Some(IntOperation::DominationNumber),
            "chromatic" | "chi" => Some(IntOperation::ChromaticNumber),
            "max_acyclic" | "acyclic" => Some(IntOperation::MaxAcyclicSubgraph),
            "clique_cover" | "theta" => Some(IntOperation::CliqueCoveringNumber),
            &_ => None,
        }
    }
}

impl BoolOperation {
    fn wrap_operation(text: &str, sep: &str, wrapper: fn(IntOperation, IntOperation) -> BoolOperation) -> Option<BoolOperation> {
        let pars: Vec<&str> = text.split(sep).map(|x| x.trim()).collect();
        if pars.len() == 2 {
            IntOperation::of_string_result(pars[0]).map(|op1| 
                IntOperation::of_string_result(pars[1]).map(|op2|
                    wrapper(op1, op2)
            )).flatten()
        } else {
            None
        }
    }

    pub fn of_string_result(text: &str) -> Option<BoolOperation> {
        if text.contains(">=") {
            fn wrapper (op1: IntOperation, op2: IntOperation) -> BoolOperation { 
                BoolOperation::NotLess(Box::new(op1), Box::new(op2))
            }
            BoolOperation::wrap_operation(text, ">=", wrapper)
        } else if text.contains("<=") {
            fn wrapper (op1: IntOperation, op2: IntOperation) -> BoolOperation { 
                BoolOperation::NotMore(Box::new(op1), Box::new(op2))
            }
            BoolOperation::wrap_operation(text, "<=", wrapper)
        } else if text.contains(">") {
            fn wrapper (op1: IntOperation, op2: IntOperation) -> BoolOperation { 
                BoolOperation::More(Box::new(op1), Box::new(op2))
            }
            BoolOperation::wrap_operation(text, ">", wrapper)
        } else if text.contains("<") {
            fn wrapper (op1: IntOperation, op2: IntOperation) -> BoolOperation { 
                BoolOperation::Less(Box::new(op1), Box::new(op2))
            }
            BoolOperation::wrap_operation(text, "<", wrapper)
        } else {
            None
        }
    }
}

impl UnitOperation {
    pub fn of_string_result(text: &str) -> Option<UnitOperation> {
        match text.trim().to_lowercase().as_str() {
            "print" => Some(UnitOperation::Print),
            &_ => None,
        }
    }
}

impl Operation {
    pub fn of_string(text: &str) -> Operation {
        IntOperation::of_string_result(text).map(|x| Operation::Int(x))
            .or(BoolOperation::of_string_result(text).map(|x| Operation::Bool(x)))
            .or(UnitOperation::of_string_result(text).map(|x| Operation::Unit(x)))
            .unwrap()
    }
}

impl fmt::Display for IntOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            IntOperation::DominationNumber => "Domination number",
            IntOperation::ChromaticNumber => "Chromatic number",
            IntOperation::MaxAcyclicSubgraph => "Max acyclic subgraph size",
            IntOperation::CliqueCoveringNumber => "Clique covering number",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for BoolOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str;
        let name = match self {
            BoolOperation::More(op1, op2) => {
                str = format!("Is ({} > {})", *op1, *op2);
                &str
            },
            BoolOperation::Less(op1, op2) => {
                str = format!("Is ({} < {})", *op1, *op2);
                &str
            },
            BoolOperation::NotMore(op1, op2) => {
                str = format!("Is ({} <= {})", *op1, *op2);
                &str
            },
            BoolOperation::NotLess(op1, op2) => {
                str = format!("Is ({} >= {})", *op1, *op2);
                &str
            },
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for UnitOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            UnitOperation::Print => "Print",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Int(op) => write!(f, "{}", op),
            Operation::Bool(op) => write!(f, "{}", op),
            Operation::Unit(op) => write!(f, "{}", op),
        }
    }
}

impl Instruction {
    pub fn of_string(text: &str) -> Instruction {
        let re = regex::Regex::new(r"->|=>").unwrap();
        let pars: Vec<&str> = re.split(text).map(|par| par.trim()).collect();
        let offset = pars.len() - 2;
        let repeats: usize = if offset > 0 { pars[0].parse().unwrap_or(1) } else { 1 };
        let constructor = Constructor::of_string(pars[offset]);
        let operations = pars[offset + 1].split(',')
            .map(|str| Operation::of_string(str)).collect();

        Instruction {
            repeats,
            constructor,
            operations,
        }
    }

    pub fn operations_string(&self) -> String {
        let operations_strs: Vec<String> = self.operations.iter().map(|x| format!("{x}")).collect();
        operations_strs.join(", ")
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Constructor: {}\nOperations: [{}]", self.constructor, self.operations_string())
    }
}