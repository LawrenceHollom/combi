use utilities::*;
use std::fmt;

use crate::pattern::*;
use crate::graph::*;

mod products;
mod erdos_renyi;
pub mod from_file;
mod grid;
mod random_planar;
mod random_regular_bipartite;
mod tree;
mod raw;
mod bowties;
mod regular;
mod semiregular;
mod corona;

#[derive(Clone)]
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

#[derive(Clone)]
pub enum RandomConstructor {
    Biregular(Order, Degree, Degree),
    ErdosRenyi(Order, f64),
    RandomBipartite(Order, f64),
    RandomSubgraph(Box<Constructor>, f64),
    Triangulation(Order),
    MaximalPlanar(Order),
    PlanarConditioned(Order, Option<Degree>, Option<usize>),
    Bowties(usize, Degree),
    Regular(Order, Degree),
    RegularIsh(Order, Degree),
    DegreeSequence(Vec<Degree>),
    PlanarGons(Order, usize),
    VertexStructured(VertexPattern, usize),
    EdgeStructured(EdgePattern, usize),
    ConnectedSemiregular(Order, f64, f64),
}

#[derive(Clone)]
pub enum RawConstructor {
    Grid(Order, Order),
    Complete(Order),
    CompleteBipartite(Order, Order),
    CompleteMultipartite(Vec<Order>),
    Turan(Order, usize),
    Cyclic(Order),
    Path(Order),
    Star(Order),
    Empty(Order),
    Cube(usize),
    FanoPlane,
    Petersen(usize, usize),
    Octahedron,
    Icosahedron,
    Dodecahedron,
}

#[derive(Clone)]
pub enum RecursiveConstructor {
    CoronaProduct(Box<Constructor>, Box<Constructor>)
}

#[derive(Clone)]
pub enum Constructor {
    Product(ProductConstructor, Box<Constructor>, Box<Constructor>),
    Random(RandomConstructor),
    Raw(RawConstructor),
    Recursive(RecursiveConstructor),
    RootedTree(Vec<usize>),
    File(String),
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
        use RecursiveConstructor::*;
        
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
            "biregular" | "bireg" => {
                let right_deg = if args.len() == 2 { args[1] } else { args[2] };
                Random(Biregular(Order::of_string(args[0]),
                    Degree::of_string(args[1]),
                    Degree::of_string(right_deg)))
            }
            "erdos_renyi" | "er" | "g" => {
                Random(ErdosRenyi(Order::of_string(args[0]), args[1].parse().unwrap()))
            },
            "random_bipartite" | "b" => {
                Random(RandomBipartite(Order::of_string(args[0]), args[1].parse().unwrap()))
            }
            "subgraph" | "sub" => {
                Random(RandomSubgraph(Box::new(Constructor::of_string(args[0])), 
                    args[1].parse().unwrap()))
            }
            "triangulation" | "tri" => {
                Random(Triangulation(Order::of_string(args[0])))
            },
            "maximal_planar" | "planar" => {
                Random(MaximalPlanar(Order::of_string(args[0])))
            },
            "planar_conditioned" | "planar_con" => {
                Random(PlanarConditioned(Order::of_string(args[0]), 
                    Some(Degree::of_string(args[1])), 
                    args.get(2).map(|x| x.parse().unwrap())))
            }
            "planar_girth" => {
                Random(PlanarConditioned(Order::of_string(args[0]), 
                    None, 
                    args.get(1).map(|x| x.parse().unwrap())))
            }
            "planar_gons" | "k_gons" => {
                Random(PlanarGons(Order::of_string(args[0]), args[1].parse().unwrap()))
            }
            "bowties" => {
                let degree = if args.len() > 1 { args[1].parse().unwrap() } else { 3 };
                Random(Bowties(args[0].parse().unwrap(), Degree::of_usize(degree)))
            }
            "regular" => {
                let degree = Degree::of_string(args[1]);
                Random(Regular(Order::of_string(args[0]), degree))
            }
            "regularish" => {
                let degree = Degree::of_string(args[1]);
                Random(RegularIsh(Order::of_string(args[0]), degree))
            }
            "sequence" | "seq" => {
                let mut deg_seq: Vec<Degree> = vec![];
                for (d_minus_one, count) in args.iter().enumerate() {
                    for _i in 0..count.parse().unwrap() {
                        deg_seq.push(Degree::of_usize(d_minus_one + 1));
                    }
                }
                Random(DegreeSequence(deg_seq))
            }
            "struct" => {
                let num = args[1].parse().unwrap();
                match VertexPattern::of_string(args[0]) {
                    Some(pattern) => Random(VertexStructured(pattern, num)),
                    None => {
                        match EdgePattern::of_string(args[0]) {
                            Some(pattern) => Random(EdgeStructured(pattern, num)),
                            None => panic!("Cannot find struct!"),
                        }
                    }
                }
            }
            "connected_semi_regular" | "conn" => {
                let exponent = if args.len() > 2 {
                        args[2].parse().unwrap()
                    } else {
                        1.0
                    };
                Random(ConnectedSemiregular(Order::of_string(args[0]), args[1].parse().unwrap(), exponent))
            }
            "grid" => {
                Raw(Grid(Order::of_string(args[0]), Order::of_string(args[1])))
            },
            "complete" | "k" => {
                let n = Order::of_string(args[0]);
                if args.len() == 1 {
                    Raw(Complete(n))
                } else if args.len() == 2 {
                    Raw(CompleteBipartite(n, Order::of_string(args[1])))
                } else {
                    Raw(CompleteMultipartite(args.iter().map(|arg| Order::of_string(arg)).collect()))
                }
            }
            "turan" | "t" => Raw(Turan(Order::of_string(args[0]), args[1].parse().unwrap())),
            "cyclic" | "c" => Raw(Cyclic(Order::of_string(args[0]))),
            "path" | "p" => Raw(Path(Order::of_string(args[0]))),
            "star" | "s" => Raw(Star(Order::of_string(args[0]))),
            "empty" | "e" => Raw(Empty(Order::of_string(args[0]))),
            "cube" | "hypercube" | "q" => Raw(Cube(args[0].parse().unwrap())),
            "fano" => Raw(FanoPlane),
            "petersen" => {
                if args.len() >= 2 {
                    Raw(Petersen(args[0].parse().unwrap(), args[1].parse().unwrap()))
                } else if !args.is_empty() {
                    Raw(Petersen(args[0].parse().unwrap(), 2))
                } else {
                    Raw(Petersen(5, 2))
                }
            }
            "octahedron" | "Eu6" => Raw(Octahedron),
            "icosahedron" | "Eu12" => Raw(Icosahedron),
            "dodecahedron" | "Eu20" => Raw(Dodecahedron),
            "corona" => {
                Recursive(CoronaProduct(Box::new(Self::of_string(args[0])), 
                    Box::new(Self::of_string(args[1]))))
            }
            str => File(str.to_owned()),
        }
    }

    pub fn new_graph(&self) -> Graph {
        use Constructor::*;
        use RandomConstructor::*;
        use RawConstructor::*;
        use RecursiveConstructor::*;

        match self {
            Product(product, c1, c2) => {
                products::new_product(product, self, &c1.new_graph(), &c2.new_graph())
            }
            RootedTree(parents) => tree::new_rooted(parents),
            Random(Biregular(order, left_deg, right_deg)) => {
                random_regular_bipartite::new_biregular(*order, *left_deg, *right_deg)
            }
            Random(ErdosRenyi(order, p)) => erdos_renyi::new(*order, *p),
            Random(RandomBipartite(order, p)) => erdos_renyi::new_bipartite(*order, *p),
            Random(RandomSubgraph(constructor, p)) => {
                erdos_renyi::new_random_subgraph(constructor, *p)
            }
            Random(Triangulation(order)) => random_planar::new_triangulation(*order),
            Random(MaximalPlanar(order)) => random_planar::new_maximal(*order),
            Random(PlanarConditioned(order, max_deg, min_girth)) => {
                random_planar::new_conditioned(*order, *max_deg, *min_girth)
            }
            Random(PlanarGons(order, k)) => random_planar::k_gon_gluing(*order, *k),
            Random(Bowties(scale, degree)) => bowties::new_bowties(*scale, *degree),
            Random(Regular(order, degree)) => regular::new_regular(order, degree),
            Random(RegularIsh(order, degree)) => regular::new_approximately_regular(order, degree),
            Random(DegreeSequence(deg_seq)) => regular::new_from_degree_sequence(deg_seq, false),
            Random(VertexStructured(pattern, num)) => pattern.new_graph(*num),
            Random(EdgeStructured(pattern, num)) => pattern.new_graph(*num),
            Random(ConnectedSemiregular(order, p, exp)) => semiregular::new_semiregular(*order, *p, *exp),
            Raw(Grid(height, width)) => grid::new(height, width),
            Raw(Complete(order)) => Graph::new_complete(*order),
            Raw(CompleteBipartite(left, right)) => Graph::new_complete_bipartite(*left, *right),
            Raw(CompleteMultipartite(orders)) => Graph::new_complete_multipartite(orders),
            Raw(Turan(order, chi)) => Graph::new_turan(*order, *chi),
            Raw(Cyclic(order)) => Graph::new_cyclic(*order),
            Raw(Path(order)) => Graph::new_path(*order),
            Raw(Star(order)) => Graph::new_star(*order),
            Raw(Empty(order)) => Graph::new_empty(*order),
            Raw(Cube(dimension)) => raw::new_cube(*dimension),
            Raw(FanoPlane) => raw::new_fano_plane(),
            Raw(Petersen(cycles, skip)) => raw::new_petersen(*cycles, *skip),
            Raw(Octahedron) => raw::new_octahedron(),
            Raw(Icosahedron) => raw::new_icosahedron(),
            Raw(Dodecahedron) => raw::new_dodecahedron(),
            Recursive(CoronaProduct(c1, c2)) => {
                corona::new_corona_product(self, &c1.new_graph(), &c2.new_graph())
            }
            File(filename) => from_file::new_graph(filename),
            Special => panic!("Cannot directly construct Special graph!"),
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
            Recursive(constr) => write!(f, "{}", constr),
            RootedTree(parents) => {
                write!(f, "Rooted tree with parent pattern {:?}", parents)
            },
            File(filename) => write!(f, "From file {}.gph", filename),
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
            RandomBipartite(order, p) => {
                write!(f, "Random bipartite graph of order {} and probability {}", order, p)
            }
            RandomSubgraph(constructor, p) => {
                write!(f, "Random subgraph of {}, keeping each edge with prob {}", **constructor, p)
            }
            Triangulation(order) => {
                write!(f, "Triangulation of order {}", order)
            }
            MaximalPlanar(order) => {
                write!(f, "Maximal planar of order {}", order)
            }
            PlanarConditioned(order, max_deg, min_girth) => {
                write!(f, "Planar with order {}, max_deg {:?}, and min_girth {:?}", order, max_deg, min_girth)
            }
            PlanarGons(order, k) => {
                write!(f, "Random gluing of {}-gons for order {}", *k, *order)
            }
            Bowties(scale, degree) => {
                let d = degree.to_usize();
                let mult = if d % 2 == 0 { d / 2 } else { d };
                write!(f, "Amalgamation of {} bowties of degree {}", mult * *scale, degree)
            }
            Regular(order, degree) => {
                write!(f, "Random regular graph of order {} and degree {}", *order, *degree)
            }
            RegularIsh(order, degree) => {
                write!(f, "Approximately random regular graph of order {} and degree {}", *order, *degree)
            }
            DegreeSequence(seq) => {
                write!(f, "Random graph with degree sequence {:?}", seq)
            }
            VertexStructured(pattern, num) => {
                write!(f, "Structured vertex-gluing of {} copies of the {} pattern", num, pattern)
            }
            EdgeStructured(pattern, num) => {
                write!(f, "Structured edge-gluing of {} copies of the {} pattern", num, pattern)
            }
            ConnectedSemiregular(order, p, exp) => {
                write!(f, "Connected semiregular graph of order {} and average degree {} with weighting exponent {}", order, p, exp)
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
            CompleteBipartite(left, right) => {
                write!(f, "Complete bipartite of order ({}, {})", left, right)
            },
            CompleteMultipartite(orders) => {
                write!(f, "Complete multipartite with part sizes {:?}", orders)
            }
            Turan(order, chi) => {
                write!(f, "The Turan graph of order {} with {} classes", order, chi)
            }
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
            Cube(dimension) => {
                write!(f, "The Cube of dimension {}", dimension)
            }
            FanoPlane => write!(f, "The Fano plane"),
            Petersen(cycles, skip) => write!(f, "The Petersen graph on {}-cycles with {}-skip", cycles, skip),
            Octahedron => write!(f, "The Octahedron"),
            Icosahedron => write!(f, "The Icosahedron"),
            Dodecahedron => write!(f, "The Dodecahedron"),
        }
    }
}

impl fmt::Display for RecursiveConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RecursiveConstructor::*;
        match self {
            CoronaProduct(c1, c2) => {
                write!(f, "Corona product of ({}) and ({})", c1, c2)
            }
        }
    }
}