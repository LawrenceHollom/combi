use std::fmt;

use utilities::*;

#[derive(Copy, Clone)]
pub enum UnitOperation {
    Print,
    RawBunkbed,
    BunkbedPosts,
    BunkbedSimulation,
    PercolationPolys,
    BunkbedCuts(usize),
    BunkbedDists,
    BunkbedDiffs(usize, Option<usize>),
    PrintIntervalColoring,
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
            "bunkbed_cut" | "bunkbed_cuts" => Some(BunkbedCuts(args[0].parse().unwrap())),
            "bunkbed_dists" => Some(BunkbedDists),
            "bunkbed_diffs" | "bb_diffs" => {
                Some(BunkbedDiffs(args[0].parse().unwrap(),
                    args.get(1).map(|x| x.parse().unwrap())))
            }
            "print_interval" | "interval" => Some(PrintIntervalColoring),
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
            Signature => "Print a bunch of graph invariants",
            Unit => "Do nothing",
        };
        write!(f, "{}", name)
    }
}