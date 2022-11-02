use utilities::*;
use std::fmt;

pub enum Constructor {
    BoxProduct(Box<Constructor>, Box<Constructor>),
    TensorProduct(Box<Constructor>, Box<Constructor>),
    LexProduct(Box<Constructor>, Box<Constructor>),
    StrongProduct(Box<Constructor>, Box<Constructor>),
    ConormalProduct(Box<Constructor>, Box<Constructor>),
    RandomRegularBipartite(Order, Degree),
    ErdosRenyi(Order, f64),
    Grid(Order, Order),
    Complete(Order),
    Cyclic(Order),
    Path(Order),
    Star(Order),
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

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
pub enum FloatOperation {
    OfInt(IntOperation),
    OfBool(BoolOperation),
    Ratio(IntOperation, IntOperation),
}

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
pub enum BoolOperation {
    More(IntOperation, IntOperation),
    Less(IntOperation, IntOperation),
    NotMore(IntOperation, IntOperation),
    NotLess(IntOperation, IntOperation),
}

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
pub enum UnitOperation {
    Print,
    RawBunkbed,
    BunkbedPosts,
}

pub enum Operation {
    Int(IntOperation),
    Bool(BoolOperation),
    Float(FloatOperation),
    Unit(UnitOperation),
}

impl Constructor {
    pub fn of_string(text: &str) -> Constructor {
        // must be otf func_tion(a, b, c, ...)
        let (func, args) = parse_function_like(text);
                
        match func.to_lowercase().as_str() {
            "cartesian" | "box" => {
                Constructor::BoxProduct(Box::new(Constructor::of_string(args[0])), 
                    Box::new(Constructor::of_string(args[1])))
            },
            "tensor" | "x" => {
                Constructor::TensorProduct(Box::new(Constructor::of_string(args[0])), 
                    Box::new(Constructor::of_string(args[1])))
            },
            "lex" | "." => {
                Constructor::LexProduct(Box::new(Constructor::of_string(args[0])), 
                    Box::new(Constructor::of_string(args[1])))
            },
            "strong" | "and" => {
                Constructor::StrongProduct(Box::new(Constructor::of_string(args[0])), 
                    Box::new(Constructor::of_string(args[1])))
            },
            "conormal" | "or" | "*" => {
                Constructor::ConormalProduct(Box::new(Constructor::of_string(args[0])), 
                    Box::new(Constructor::of_string(args[1])))
            },
            "rrb" | "random_regular_bipartite" => {
                Constructor::RandomRegularBipartite(Order::of_string(args[0]), 
                    Degree::of_string(args[1]))
            },
            "erdos_renyi" | "er" | "g" => {
                Constructor::ErdosRenyi(Order::of_string(args[0]), args[1].parse().unwrap())
            },
            "grid" => {
                Constructor::Grid(Order::of_string(args[0]), Order::of_string(args[1]))
            },
            "complete" | "k" => Constructor::Complete(Order::of_string(args[0])),
            "cyclic" | "c" => Constructor::Cyclic(Order::of_string(args[0])),
            "path" | "p" => Constructor::Path(Order::of_string(args[0])),
            "star" | "s" => Constructor::Star(Order::of_string(args[0])),
            "fano" => Constructor::FanoPlane,
            "petersen" => Constructor::Petersen,
            &_ => panic!("Could not find graph constructor!"),
        }
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constructor::BoxProduct(constr1, constr2) => {
                write!(f, "Box product of ({}) and ({})", constr1, constr2)
            },
            Constructor::TensorProduct(constr1, constr2) => {
                write!(f, "Tensor product of ({}) and ({})", constr1, constr2)
            },
            Constructor::LexProduct(constr1, constr2) => {
                write!(f, "Lexicographical product of ({}) and ({})", constr1, constr2)
            },
            Constructor::StrongProduct(constr1, constr2) => {
                write!(f, "Strong product of ({}) and ({})", constr1, constr2)
            },
            Constructor::ConormalProduct(constr1, constr2) => {
                write!(f, "Conormal product of ({}) and ({})", constr1, constr2)
            },
            Constructor::RandomRegularBipartite(order, deg) => {
                write!(f, "Random regular bipartite or order {} and degree {}", order, deg)
            },
            Constructor::ErdosRenyi(order, p) => {
                write!(f, "Erdos-Renyi random graph of order {} with probability {}", order, p)
            },
            Constructor::Grid(height, width) => {
                write!(f, "Grid graph on {} x {} vertices", height, width)
            }
            Constructor::Complete(order) => {
                write!(f, "Complete of order {}", order)
            },
            Constructor::Cyclic(order) => {
                write!(f, "Cyclic of order {}", order)
            },
            Constructor::Path(order) => {
                write!(f, "Path of order {}", order)
            },
            Constructor::Star(order) => {
                write!(f, "Star of order {}", order)
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
                BoolOperation::NotLess(op1, op2)
            }
            BoolOperation::wrap_operation(text, ">=", wrapper)
        } else if text.contains("<=") {
            fn wrapper (op1: IntOperation, op2: IntOperation) -> BoolOperation { 
                BoolOperation::NotMore(op1, op2)
            }
            BoolOperation::wrap_operation(text, "<=", wrapper)
        } else if text.contains(">") {
            fn wrapper (op1: IntOperation, op2: IntOperation) -> BoolOperation { 
                BoolOperation::More(op1, op2)
            }
            BoolOperation::wrap_operation(text, ">", wrapper)
        } else if text.contains("<") {
            fn wrapper (op1: IntOperation, op2: IntOperation) -> BoolOperation { 
                BoolOperation::Less(op1, op2)
            }
            BoolOperation::wrap_operation(text, "<", wrapper)
        } else {
            None
        }
    }
}

impl FloatOperation {
    fn of_string_result(text: &str) -> Option<FloatOperation> {
        if text.contains("/") {
            let pars: Vec<&str> = text.split("/").map(|x| x.trim()).collect();
            if pars.len() == 2 {
                IntOperation::of_string_result(pars[0]).map(|op1| 
                    IntOperation::of_string_result(pars[1]).map(|op2|
                        FloatOperation::Ratio(op1, op2)
                )).flatten()
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl UnitOperation {
    pub fn of_string_result(text: &str) -> Option<UnitOperation> {
        match text.trim().to_lowercase().as_str() {
            "print" => Some(UnitOperation::Print),
            "bunkbed" => Some(UnitOperation::RawBunkbed),
            "bunkbed_posts" | "posts" => Some(UnitOperation::BunkbedPosts),
            &_ => None,
        }
    }
}

impl Operation {
    pub fn of_string(text: &str) -> Operation {
        IntOperation::of_string_result(text).map(|x| Operation::Int(x))
            .or(BoolOperation::of_string_result(text).map(|x| Operation::Bool(x)))
            .or(FloatOperation::of_string_result(text).map(|x| Operation::Float(x)))
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
        let name = match self {
            BoolOperation::More(op1, op2) => 
                format!("Is ({} > {})", *op1, *op2),
            BoolOperation::Less(op1, op2) => 
                format!("Is ({} < {})", *op1, *op2),
            BoolOperation::NotMore(op1, op2) => 
                format!("Is ({} <= {})", *op1, *op2),
            BoolOperation::NotLess(op1, op2) => 
                format!("Is ({} >= {})", *op1, *op2),
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for FloatOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            FloatOperation::OfBool(op) => format!("Of Bool ({})", op),
            FloatOperation::OfInt(op) => format!("Of int ({})", op),
            FloatOperation::Ratio(op1, op2) => format!("Ratio ({}) / ({})", op1, op2),
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for UnitOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            UnitOperation::Print => "Print",
            UnitOperation::RawBunkbed => "Raw bunkbed",
            UnitOperation::BunkbedPosts => "Bunkbed posts",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Int(op) => write!(f, "{}", op),
            Operation::Bool(op) => write!(f, "{}", op),
            Operation::Float(op) => write!(f, "{}", op),
            Operation::Unit(op) => write!(f, "{}", op),
        }
    }
}