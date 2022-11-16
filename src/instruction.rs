use utilities::*;
use std::fmt;

#[derive(Copy, Clone)]
pub enum ProductConstructor {
    Cartesian,
    Tensor,
    Lex,
    RevLex,
    Strong,
    Conormal,
    Rooted,
    RevRooted,
}

#[derive(Copy, Clone)]
pub enum RandomConstructor {
    RegularBipartite(Order, Degree),
    ErdosRenyi(Order, f64),
}

#[derive(Copy, Clone)]
pub enum RawConstructor {
    Grid(Order, Order),
    Complete(Order),
    Cyclic(Order),
    Path(Order),
    Star(Order),
    Empty(Order),
    FanoPlane,
    Petersen,
}

pub enum Constructor {
    Product(ProductConstructor, Box<Constructor>, Box<Constructor>),
    Random(RandomConstructor),
    Raw(RawConstructor),
    RootedTree(Vec<usize>),
}

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
pub enum IntOperation {
    Order,
    Size,
    LargestComponent,
    NumComponents,
    DominationNumber,
    ChromaticNumber,
    MaxAcyclicSubgraph,
    CliqueCoveringNumber,
    NumOnLongMonotone,
    MaxMonotonePath,
    TotalDominationGameLength,
    Number(u32),
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
    IsConnected,
    HasLongMonotone,
}

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
pub enum UnitOperation {
    Print,
    RawBunkbed,
    BunkbedPosts,
    BunkbedSimulation,
    Unit,
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
        use Constructor::*;
        use ProductConstructor::*;
        use RawConstructor::*;
        use RandomConstructor::*;
                
        match func.to_lowercase().as_str() {
            "cartesian" | "box" => {
                Product(Cartesian, Box::new(Self::of_string(args[0])), 
                    Box::new(Self::of_string(args[1])))
            },
            "tensor" | "x" => {
                Product(Tensor, Box::new(Self::of_string(args[0])), 
                    Box::new(Self::of_string(args[1])))
            },
            "lex" | "." => {
                Product(Lex, Box::new(Self::of_string(args[0])), 
                    Box::new(Self::of_string(args[1])))
            },
            "strong" | "and" => {
                Product(Strong, Box::new(Self::of_string(args[0])), 
                    Box::new(Self::of_string(args[1])))
            },
            "conormal" | "or" | "*" => {
                Product(Conormal, Box::new(Self::of_string(args[0])), 
                    Box::new(Self::of_string(args[1])))
            },
            "rrb" | "random_regular_bipartite" => {
                Random(RegularBipartite(Order::of_string(args[0]), 
                    Degree::of_string(args[1])))
            },
            "erdos_renyi" | "er" | "g" => {
                Random(ErdosRenyi(Order::of_string(args[0]), args[1].parse().unwrap()))
            },
            "grid" => {
                Raw(Grid(Order::of_string(args[0]), Order::of_string(args[1])))
            },
            "complete" | "k" => Raw(Complete(Order::of_string(args[0]))),
            "cyclic" | "c" => Raw(Cyclic(Order::of_string(args[0]))),
            "path" | "p" => Raw(Path(Order::of_string(args[0]))),
            "star" | "s" => Raw(Star(Order::of_string(args[0]))),
            "empty" | "e" => Raw(Empty(Order::of_string(args[0]))),
            "fano" => Raw(FanoPlane),
            "petersen" => Raw(Petersen),
            &_ => panic!("Could not find graph constructor!"),
        }
    }

    pub fn clone(&self) -> Constructor {
        use Constructor::*;
        match self {
            Product(constr, c1, c2) => 
                Product(constr.clone(), Box::new((*c1).clone()), Box::new((*c2).clone())),
            Random(constr) => Random(constr.clone()),
            Raw(constr) => Raw(constr.clone()),
            RootedTree(parents) => RootedTree(parents.to_owned()),
        }
    }
}

impl ProductConstructor {
    pub fn all() -> Vec<ProductConstructor> {
        use ProductConstructor::*;
        vec![Cartesian, Tensor, Lex, RevLex, Strong, Conormal, Rooted, RevRooted]
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Constructor::*;
        match self {
            Product(product, constr1, constr2) => {
                write!(f, "{} product of ({}) and ({})", product, constr1, constr2)
            },
            Random(constr) => write!(f, "Random({})", constr),
            Raw(constr) => write!(f, "{}", constr),
            Constructor::RootedTree(parents) => {
                write!(f, "Rooted tree with parent pattern {:?}", parents)
            },
        }
    }
}

impl fmt::Display for ProductConstructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ProductConstructor::*;
        let str = 
            match self {
                Cartesian => "Box",
                Tensor => "Tensor",
                Lex => "Lex",
                RevLex => "Reverse Lex",
                Strong => "Strong",
                Conormal => "Conormal",
                Rooted => "Rooted",
                RevRooted => "Reverse Rooted",
            };
        write!(f, "{}", str)
    }
}

impl fmt::Display for RandomConstructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RandomConstructor::*;
        match self {
            RegularBipartite(order, deg) => {
                write!(f, "Regular bipartite or order {} and degree {}", order, deg)
            },
            ErdosRenyi(order, p) => {
                write!(f, "Erdos-Renyi graph of order {} with probability {}", order, p)
            },
        }
    }
}

impl fmt::Display for RawConstructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RawConstructor::*;
        match self {
            Grid(height, width) => {
                write!(f, "Grid graph on {} x {} vertices", height, width)
            }
            Complete(order) => {
                write!(f, "Complete of order {}", order)
            },
            Cyclic(order) => {
                write!(f, "Cyclic of order {}", order)
            },
            Path(order) => {
                write!(f, "Path of order {}", order)
            },
            Star(order) => {
                write!(f, "Star of order {}", order)
            },
            Empty(order) => {
                write!(f, "Empty of order {}", order)
            },
            FanoPlane => write!(f, "the Fano plane"),
            Petersen => write!(f, "the Petersen graph"),
        }
    }
}

impl IntOperation {
    pub fn of_string_result(text: &str) -> Option<IntOperation> {
        match text.trim().to_lowercase().as_str() {
            "order" | "n" => Some(IntOperation::Order),
            "size" | "edges" => Some(IntOperation::Size),
            "largest_component" | "largest" => Some(IntOperation::LargestComponent),
            "num_components" | "comps" => Some(IntOperation::NumComponents),
            "domination" | "dominator" | "gamma" => Some(IntOperation::DominationNumber),
            "chromatic" | "chi" => Some(IntOperation::ChromaticNumber),
            "max_acyclic" | "acyclic" => Some(IntOperation::MaxAcyclicSubgraph),
            "clique_cover" | "theta" => Some(IntOperation::CliqueCoveringNumber),
            "num_on_long_monotone" | "num_monot" => Some(IntOperation::NumOnLongMonotone),
            "max_monot" => Some(IntOperation::MaxMonotonePath),
            "total_domination_game" | "gamma_tg" => Some(IntOperation::TotalDominationGameLength),
            str => str.parse().ok().map(|x| IntOperation::Number(x)),
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
            match text.trim().to_lowercase().as_str() {
                "is_connected" | "connected" => Some(BoolOperation::IsConnected),
                "has_long_monotone" | "has_monot" | "is_monot" => Some(BoolOperation::HasLongMonotone),
                &_ => None,
            }
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
            "bunkbed_sim" => Some(UnitOperation::BunkbedSimulation),
            "()" => Some(UnitOperation::Unit),
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
        let sta;
        let name = match self {
            IntOperation::Order => "Order",
            IntOperation::Size => "Size",
            IntOperation::LargestComponent => "Largest component",
            IntOperation::NumComponents => "Number of components",
            IntOperation::DominationNumber => "Domination number",
            IntOperation::ChromaticNumber => "Chromatic number",
            IntOperation::MaxAcyclicSubgraph => "Max acyclic subgraph size",
            IntOperation::CliqueCoveringNumber => "Clique covering number",
            IntOperation::NumOnLongMonotone => "Number of vertices on a 1->n monotone path",
            IntOperation::MaxMonotonePath => "Length of longest monotone path",
            IntOperation::TotalDominationGameLength => "Length of the total domination game",
            IntOperation::Number(n) => {
                sta = n.to_string();
                sta.as_str()
            }
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
            BoolOperation::IsConnected => "Is connected".to_owned(),
            BoolOperation::HasLongMonotone => "Has 1->n monotone path".to_owned()
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
            UnitOperation::BunkbedSimulation => "Bunkbed simulation",
            UnitOperation::Unit => "Do nothing",
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