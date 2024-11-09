use utilities::*;
use std::fmt;

use crate::pattern::*;
use crate::entity::*;
use crate::entity::graph::*;
use crate::entity::poset::*;
use crate::entity::digraph::*;

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
mod random_bfs;
mod strucutral;
mod random_posets;
mod random_digraphs;

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
    BasedErdosRenyi(Order, f64, Box<Constructor>),
    InducedErdosRenyi(Order, f64, Box<Constructor>),
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
    BFSOptimal(Order, usize, f64),
    Spinal(Order, f64, Degree),
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
    CoronaProduct(Box<Constructor>, Box<Constructor>),
}

#[derive(Clone)]
pub enum StructuralConstructor {
    GiantComponent,
    TwoCore,
}

#[derive(Clone)]
pub enum PosetConstructor {
    Chain(Order),
    Antichain(Order),
    ChainIntersection(Order, usize),
    CorrelatedIntersection(Order, usize, f64),
}

#[derive(Clone)]
pub enum DigraphConstructor {
    OfGraph(Box<Constructor>),
    Oriented(Order, f64, f64),
}

#[derive(Clone)]
pub enum Constructor {
    Product(ProductConstructor, Box<Constructor>, Box<Constructor>),
    Random(RandomConstructor),
    Raw(RawConstructor),
    Recursive(RecursiveConstructor),
    Structural(StructuralConstructor, Box<Constructor>),
    RootedTree(Vec<usize>),
    PosetConstr(PosetConstructor),
    DigraphConstr(DigraphConstructor),
    File(String),
    Serialised(String),
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
        use StructuralConstructor::*;
        use PosetConstructor::*;
        use DigraphConstructor::*;
        
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
                let n = Order::of_string(args[0]);
                let p: f64 = args[1].parse().unwrap();
                if args.len() == 3 {
                    Random(BasedErdosRenyi(n, p, Box::new(Constructor::of_string(args[2]))))
                } else {
                    Random(ErdosRenyi(n, p))
                }
            },
            "g_ind" => {
                Random(InducedErdosRenyi(Order::of_string(args[0]), args[1].parse().unwrap(), 
                    Box::new(Constructor::of_string(args[2]))))
            }
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
            "bfs" => Random(BFSOptimal(Order::of_string(args[0]), args[1].parse().unwrap(), args[2].parse().unwrap())),
            "spine" | "spinal" => {
                let spine_proportion = args.get(1).map_or(0.667, |x| x.parse().unwrap());
                let off_vert_degree = args.get(2).map_or(Degree::of_usize(3), |x| Degree::of_string(x));
                Random(Spinal(Order::of_string(args[0]), spine_proportion, off_vert_degree))
            }
            "giant" => Structural(GiantComponent, Box::new(Self::of_string(args[0]))),
            "2core" | "core" | "two_core" => Structural(TwoCore, Box::new(Self::of_string(args[0]))),
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
            "serial" => Serialised(args[0].to_string()),
            "chain" => PosetConstr(Chain(Order::of_string(args[0]))),
            "antichain" | "anti" => PosetConstr(Antichain(Order::of_string(args[0]))),
            "intersection" | "inter" => {
                let order = Order::of_string(args[0]);
                let k = args[1].parse().unwrap();
                PosetConstr(ChainIntersection(order, k))
            }
            "correlated_inter" => {
                let order = Order::of_string(args[0]);
                let k = args[1].parse().unwrap();
                let prob = args[2].parse().unwrap();
                PosetConstr(CorrelatedIntersection(order, k, prob))
            }
            "digraph" => DigraphConstr(OfGraph(Box::new(Self::of_string(args[0])))),
            "oriented" => {
                let min_p = args.get(1).map_or(0.0, |str| str.parse().unwrap());
                let max_p = args.get(2).map_or(1.0, |str| str.parse().unwrap());
                DigraphConstr(Oriented(Order::of_string(args[0]), min_p, max_p))
            }
            str => File(str.to_owned()),
        }
    }

    pub fn new_entity(&self) -> Entity {
        use Constructor::*;
        use RandomConstructor::*;
        use RawConstructor::*;
        use RecursiveConstructor::*;
        use StructuralConstructor::*;
        use PosetConstructor::*;
        use DigraphConstructor::*;

        fn g(g: Graph) -> Entity {
            Entity::Graph(g)
        }
        fn p(p: Poset) -> Entity {
            Entity::Poset(p)
        }
        fn d(d: Digraph) -> Entity {
            Entity::Digraph(d)
        }

        match self {
            Product(product, c1, c2) => {
                let g1 = c1.new_entity().as_owned_graph();
                let g2 = c2.new_entity().as_owned_graph();
                g(products::new_product(product, self, &g1, &g2))
            }
            RootedTree(parents) => g(tree::new_rooted(parents)),
            Random(Biregular(order, left_deg, right_deg)) => {
                g(random_regular_bipartite::new_biregular(*order, *left_deg, *right_deg))
            }
            Random(ErdosRenyi(order, p)) => g(erdos_renyi::new(*order, *p)),
            Random(BasedErdosRenyi(order, p, base)) => g(erdos_renyi::new_based(*order, *p, base)),
            Random(InducedErdosRenyi(order, p, base)) => g(erdos_renyi::new_induced(*order, *p, base)),
            Random(RandomBipartite(order, p)) => g(erdos_renyi::new_bipartite(*order, *p)),
            Random(RandomSubgraph(constructor, p)) => {
                g(erdos_renyi::new_random_subgraph(constructor, *p))
            }
            Random(Triangulation(order)) => g(random_planar::new_triangulation(*order)),
            Random(MaximalPlanar(order)) => g(random_planar::new_maximal(*order)),
            Random(PlanarConditioned(order, max_deg, min_girth)) => {
                g(random_planar::new_conditioned(*order, *max_deg, *min_girth))
            }
            Random(PlanarGons(order, k)) => g(random_planar::k_gon_gluing(*order, *k)),
            Random(Bowties(scale, degree)) => g(bowties::new_bowties(*scale, *degree)),
            Random(Regular(order, degree)) => g(regular::new_regular(order, degree)),
            Random(RegularIsh(order, degree)) => g(regular::new_approximately_regular(order, degree)),
            Random(DegreeSequence(deg_seq)) => g(regular::new_from_degree_sequence(deg_seq, false)),
            Random(VertexStructured(pattern, num)) => g(pattern.new_graph(*num)),
            Random(EdgeStructured(pattern, num)) => g(pattern.new_graph(*num)),
            Random(ConnectedSemiregular(order, p, exp)) => g(semiregular::new_semiregular(*order, *p, *exp)),
            Random(BFSOptimal(order, width, density)) => g(random_bfs::new_bfs(*order, *width, *density)),
            Random(Spinal(order, spine_propn, off_deg)) => g(random_bfs::new_spinal(*order, *spine_propn, off_deg)),
            Raw(Grid(height, width)) => g(grid::new(height, width)),
            Raw(Complete(order)) => g(Graph::new_complete(*order)),
            Raw(CompleteBipartite(left, right)) => g(Graph::new_complete_bipartite(*left, *right)),
            Raw(CompleteMultipartite(orders)) => g(Graph::new_complete_multipartite(orders)),
            Raw(Turan(order, chi)) => g(Graph::new_turan(*order, *chi)),
            Raw(Cyclic(order)) => g(Graph::new_cyclic(*order)),
            Raw(Path(order)) => g(Graph::new_path(*order)),
            Raw(Star(order)) => g(Graph::new_star(*order)),
            Raw(Empty(order)) => g(Graph::new_empty(*order)),
            Raw(Cube(dimension)) => g(raw::new_cube(*dimension)),
            Raw(FanoPlane) => g(raw::new_fano_plane()),
            Raw(Petersen(cycles, skip)) => g(raw::new_petersen(*cycles, *skip)),
            Raw(Octahedron) => g(raw::new_octahedron()),
            Raw(Icosahedron) => g(raw::new_icosahedron()),
            Raw(Dodecahedron) => g(raw::new_dodecahedron()),
            Recursive(CoronaProduct(c1, c2)) => {
                let g1 = c1.new_entity().as_owned_graph();
                let g2 = c2.new_entity().as_owned_graph();
                g(corona::new_corona_product(self, &g1, &g2))
            }
            Structural(GiantComponent, c) => {
                let g1 = c.new_entity().as_owned_graph();
                g(strucutral::giant_component(g1))
            }
            Structural(TwoCore, c) => {
                let g1 = c.new_entity().as_owned_graph();
                g(strucutral::two_core(g1))
            }
            PosetConstr(Chain(order)) => p(Poset::new_chain(*order)),
            PosetConstr(Antichain(order)) => p(Poset::new_antichain(*order)),
            PosetConstr(ChainIntersection(order, k)) => p(random_posets::new_random_intersection(*order, *k)),
            PosetConstr(CorrelatedIntersection(order, k, prob)) => p(random_posets::new_correlated_intersection(*order, *k, *prob)),
            DigraphConstr(OfGraph(constr)) => {
                let g = constr.new_entity().as_owned_graph();
                d(Digraph::of_matrix(g.adj, vec![]))
            }
            DigraphConstr(Oriented(order, min_p, max_p)) => d(random_digraphs::new_oriented(*order, *min_p, *max_p)),
            File(filename) => from_file::new_entity(filename),
            Serialised(code) => g(Graph::deserialise(code)),
            Special => panic!("Cannot directly construct Special graph!"),
        }
    }

    pub fn is_random(&self) -> bool {
        use Constructor::*;
        match self {
            Product(_, c1, c2) => {
                c1.is_random() || c2.is_random()
            }
            Recursive(RecursiveConstructor::CoronaProduct(c1, c2)) => {
                c1.is_random() || c2.is_random()
            }
            Structural(_, c) => c.is_random(),
            PosetConstr(poset_constr) => poset_constr.is_random(),
            DigraphConstr(digraph_constr) => digraph_constr.is_random(),
            RootedTree(_) | Raw(_) | File(_) | Serialised(_) | Special => false,
            Random(_) => true,
        }
    }
}

impl ProductConstructor {
    pub fn all() -> Vec<ProductConstructor> {
        use ProductConstructor::*;
        vec![Cartesian, Tensor, Lex, RevLex, Strong, Conormal, Rooted, RevRooted]
    }
}

impl PosetConstructor {
    pub fn is_random(&self) -> bool {
        use PosetConstructor::*;
        match self {
            Chain(_) | Antichain(_) => false,
            ChainIntersection(_, _) | CorrelatedIntersection(_, _, _) => true,
        }
    }
}

impl DigraphConstructor {
    pub fn is_random(&self) -> bool {
        use DigraphConstructor::*;
        match self {
            OfGraph(constr) => constr.is_random(),
            Oriented(_, _, _) => true,
        }
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
            Structural(constr, c1) => write!(f, "Structural({}, {})", constr, c1),
            RootedTree(parents) => {
                write!(f, "Rooted tree with parent pattern {:?}", parents)
            },
            PosetConstr(constr) => write!(f, "{}", constr),
            DigraphConstr(constr) => write!(f, "{}", constr),
            File(filename) => write!(f, "From file {}", filename),
            Serialised(code) => write!(f, "From code {}", code),
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
            BasedErdosRenyi(order, p, base) => {
                write!(f, "Erdos-Renyi graph of order {} with prob {} on top of {}", order, p, *base)
            },
            InducedErdosRenyi(order, p, base) => {
                write!(f, "Erdos-Renyi graph of order {} with prob {} with induced {}", order, p, *base)
            }
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
            BFSOptimal(order, width, density) => {
                write!(f, "BFS optimised of order {}, edge-width {}, and edge-density {}", order, width, density)
            }
            Spinal(order, spine_propn, off_deg) => {
                write!(f, "Spinal graph of order {}, with {:.3}-propn of edges in the spine. Off-spine degree = {}", order, spine_propn, off_deg)
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

impl fmt::Display for StructuralConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use StructuralConstructor::*;
        match self {
            GiantComponent => write!(f, "Giant component"),
            TwoCore => write!(f, "2-core"),
        }
    }
}

impl fmt::Display for PosetConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PosetConstructor::*;
        match self {
            Chain(order) => write!(f, "Chain of {} elements", order),
            Antichain(order) => write!(f, "Antichain of {} elements", order),
            ChainIntersection(order, k) => {
                write!(f, "Intersection of {} random chains of {} elements", k, order)
            }
            CorrelatedIntersection(order, k, prob) => {
                write!(f, "Intersection of {} correlated (prob {} of swaps) random chains of {} elements", k, prob, order)
            }
        }
    }
}

impl fmt::Display for DigraphConstructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use DigraphConstructor::*;
        match self {
            OfGraph(digraph) => write!(f, "Digraph of ({})", *digraph),
            Oriented(n, min_p, max_p) => write!(f, "Random orientation of G({}, [{}, {}])", n, min_p, max_p),
        }
    }
}