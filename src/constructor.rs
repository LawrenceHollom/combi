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
    Biregular(Order, Degree, Degree),
    ErdosRenyi(Order, f64),
    Triangulation(Order),
    MaximalPlanar(Order),
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
    Cube,
    Octahedron,
    Icosahedron,
    Dodecahedron,
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
                let deg = Degree::of_string(args[1]);
                Random(Biregular(Order::of_string(args[0]), deg, deg))
            },
            "biregular" => {
                Random(Biregular(Order::of_string(args[0]),
                    Degree::of_string(args[1]),
                    Degree::of_string(args[2])))
            }
            "erdos_renyi" | "er" | "g" => {
                Random(ErdosRenyi(Order::of_string(args[0]), args[1].parse().unwrap()))
            },
            "triangulation" | "tri" => {
                Random(Triangulation(Order::of_string(args[0])))
            },
            "maximal_planar" | "planar" => {
                Random(MaximalPlanar(Order::of_string(args[0])))
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
            "cube" | "Eu8" => Raw(Cube),
            "octahedron" | "Eu6" => Raw(Octahedron),
            "icosahedron" | "Eu12" => Raw(Icosahedron),
            "dodecahedron" | "Eu20" => Raw(Icosahedron),
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
            Biregular(order, left_deg, right_deg) => {
                write!(f, "Biregular of left-order {}, left-degree {}, and right-degree {}", order, left_deg, right_deg)
            },
            ErdosRenyi(order, p) => {
                write!(f, "Erdos-Renyi graph of order {} with probability {}", order, p)
            },
            Triangulation(order) => {
                write!(f, "Triangulation of order {}", order)
            }
            MaximalPlanar(order) => {
                write!(f, "Maximal planar of order {}", order)
            }
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
            FanoPlane => write!(f, "The Fano plane"),
            Petersen => write!(f, "The Petersen graph"),
            Cube => write!(f, "The Cube"),
            Octahedron => write!(f, "The Octahedron"),
            Icosahedron => write!(f, "The Icosahedron"),
            Dodecahedron => write!(f, "The Dodecahedron"),
        }
    }
}