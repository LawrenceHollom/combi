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

#[derive(Clone)]
pub enum Constructor {
    Product(ProductConstructor, Box<Constructor>, Box<Constructor>),
    Random(RandomConstructor),
    Raw(RawConstructor),
    RootedTree(Vec<usize>),
    Special,
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
            "rooted" => {
                Product(Rooted, Box::new(Self::of_string(args[0])), 
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
            RootedTree(parents) => {
                write!(f, "Rooted tree with parent pattern {:?}", parents)
            },
            Special => write!(f, "Special"),
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