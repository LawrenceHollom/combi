use std::fmt;

use utilities::*;
use utilities::vertex_tools::*;

#[derive(Copy, Clone)]
pub enum UnitOperation {
    Print,
    RawBunkbed,
    BunkbedPosts,
    BunkbedSimulation,
    PercolationPolys,
    BunkbedCuts(Vertex),
    BunkbedDists,
    BunkbedDiffs(Vertex, Option<usize>),
    PrintIntervalColoring,
    PrintDominatingSet,
    Signature,
    Unit,
}

impl UnitOperation {
    pub fn of_string_result(text: &str) -> Option<UnitOperation> {
        use UnitOperation::*;
        let (func, args) = parse_function_like(text);
        match func.trim().to_lowercase().as_str() {
            "print" => Some(Print),
            "bunkbed" => Some(RawBunkbed),
            "bunkbed_posts" | "posts" => Some(BunkbedPosts),
            "bunkbed_sim" => Some(BunkbedSimulation),
            "percolate" => Some(PercolationPolys),
            "bunkbed_cut" | "bunkbed_cuts" => Some(BunkbedCuts(Vertex::of_string(args[0]))),
            "bunkbed_dists" => Some(BunkbedDists),
            "bunkbed_diffs" | "bb_diffs" => {
                Some(BunkbedDiffs(Vertex::of_string(args[0]),
                    args.get(1).map(|x| x.parse().unwrap())))
            }
            "print_interval" | "interval" => Some(PrintIntervalColoring),
            "print_dominator" | "print_gamma" => Some(PrintDominatingSet),
            "signature" | "sig" => Some(Signature),
            "()" | "(" => Some(Unit),
            &_ => None,
        }
    }
}

impl fmt::Display for UnitOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnitOperation::*;
        let str: String;
        let name = match self {
            Print => "Print",
            RawBunkbed => "Raw bunkbed",
            BunkbedPosts => "Bunkbed posts",
            BunkbedSimulation => "Bunkbed simulation",
            PercolationPolys => "Percolate",
            BunkbedCuts(u) => { str = format!("Bunkbed cuts to {}", *u); &str }
            BunkbedDists => "Bunkbed distance-bounded polynomials",
            BunkbedDiffs(_u, _size) => "Print info about interesting bunkbed configs",
            PrintIntervalColoring => "Print an interval coloring (if exists)",
            PrintDominatingSet => "Print a random-ish minimal dominating set",
            Signature => "Print a bunch of graph invariants",
            Unit => "Do nothing",
        };
        write!(f, "{}", name)
    }
}