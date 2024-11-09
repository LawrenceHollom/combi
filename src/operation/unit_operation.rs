use std::fmt;

use utilities::*;
use utilities::vertex_tools::*;

#[derive(Copy, Clone)]
pub enum UnitOperation {
    Print,
    PrintMatrix,
    RawBunkbed,
    BunkbedPosts,
    BunkbedSimulation,
    PercolationPolys,
    BunkbedCuts(Vertex),
    BunkbedDists,
    BunkbedDiffs(Vertex, Option<usize>),
    PrintIntervalColoring,
    PrintDominatingSet,
    GameChromaticTable,
    GameChromaticStrategy(usize),
    ConnectedGameChromaticStrategy(usize),
    PrintAutomorphisms,
    BunkbedPostRemoval,
    PrintMarkingGameStrat,
    PrintConnectedMarkingGameStrat,
    PrintBobWinGrabbingWeighting,
    GrabbingHypothesisTest,
    BunkbedSiteCOunts,
    BunkbedReducedConnectionCounts(usize),
    BunkbedReducedConnectionSimulation(usize, usize),
    BunkbedReducedConnectionDP(usize, usize, usize),
    BunkbedCounterexampleSearch(usize),
    PosetRelationProbabilities,
    PosetHasseDiagram,
    BalancedHeuristics,
    PosetPrintBalanceAsCap,
    PrintNorineHypercubeColourings,
    PrintSymmetricNorineHypercubeColourings(usize, usize),
    PrintMinKernel,
    Signature,
    Unit,
}

impl UnitOperation {
    pub fn of_string_result(text: &str) -> Option<UnitOperation> {
        use UnitOperation::*;
        let (func, args) = parse_function_like(text);
        match func.trim().to_lowercase().as_str() {
            "print" => Some(Print),
            "print_matrix" | "matrix" => Some(PrintMatrix),
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
            "game_chromatic_table" | "chi_g_table" | "chi_g_t" => Some(GameChromaticTable),
            "game_chromatic_strategy" | "chi_g_strat" => Some(GameChromaticStrategy(args[0].parse().unwrap())),
            "chi_cg_strat" => Some(ConnectedGameChromaticStrategy(args[0].parse().unwrap())),
            "print_automorphisms" | "print_autos" => Some(PrintAutomorphisms),
	        "bunkbed_post_removal" | "bb_p_r" => Some(BunkbedPostRemoval),
            "signature" | "sig" => Some(Signature),
            "mark_strat" => Some(PrintMarkingGameStrat),
            "mark_c_strat" => Some(PrintConnectedMarkingGameStrat),
            "print_bgw" => Some(PrintBobWinGrabbingWeighting),
            "grabby" => Some(GrabbingHypothesisTest),
            "bunkbed_sites" | "bb_sites" => Some(BunkbedSiteCOunts),
            "bunkbed_connections" | "bbrcc" => Some(BunkbedReducedConnectionCounts(args[0].parse().unwrap())),
            "bunkbed_connection_sims" | "bbcs" => {
                let k = args.get(1).map_or(2, |k| k.parse().unwrap());
                Some(BunkbedReducedConnectionSimulation(args[0].parse().unwrap(), k))
            },
            "bunkbed_connection_dp" | "bbcdp" => {
                let k = args.get(1).map_or(2, |k| k.parse().unwrap());
                let edges = args.get(2).map_or(0, |x| x.parse().unwrap());
                Some(BunkbedReducedConnectionDP(args[0].parse().unwrap(), k, edges))
            },
            "bb_counterexample" => Some(BunkbedCounterexampleSearch(args.get(0).map_or(1, |x| x.parse().unwrap()))),
            "print_balance" => Some(PosetRelationProbabilities),
            "hasse" => Some(PosetHasseDiagram),
            "balance_heuristics" => Some(BalancedHeuristics),
            "print_cap_balance" => Some(PosetPrintBalanceAsCap),
            "print_norine" => Some(PrintNorineHypercubeColourings),
            "print_symm_norine" => {
                let slice = args.get(0).map_or(0, |str| str.parse().unwrap());
                let out_of = args.get(1).map_or(0, |str| str.parse().unwrap());
                Some(PrintSymmetricNorineHypercubeColourings(slice, out_of))
            }
            "print_min_kernel" => Some(PrintMinKernel),
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
            PrintMatrix => "Print adjacency matrix",
            RawBunkbed => "Raw bunkbed",
            BunkbedPosts => "Bunkbed posts",
            BunkbedSimulation => "Bunkbed simulation",
            PercolationPolys => "Percolate",
            BunkbedCuts(u) => { 
                str = format!("Bunkbed cuts to {}", *u); 
                &str 
            }
            BunkbedDists => "Bunkbed distance-bounded polynomials",
            BunkbedDiffs(_u, _size) => "Print info about interesting bunkbed configs",
            PrintIntervalColoring => "Print an interval coloring (if exists)",
            PrintDominatingSet => "Print a random-ish minimal dominating set",
            GameChromaticTable => "Print winners of chromatic game for various k",
            GameChromaticStrategy(k) => { 
                str = format!("Strategy for vertex colouring game for {} colours", k); 
                &str 
            }
            ConnectedGameChromaticStrategy(k) => {
                str = format!("Strategy for connected vertex colouring game for {} colours", k);
                &str
            }
            PrintAutomorphisms => "Print automorphisms of g, and other autoj metadata",
	        BunkbedPostRemoval => "Tests bunkbed induction via removing posts",
            Signature => "Print a bunch of invariants",
            PrintMarkingGameStrat => "Print marking game strategy",
            PrintConnectedMarkingGameStrat => "Print connected marking game strategy",
            PrintBobWinGrabbingWeighting => "Print a weighting with which Bob wins the graph grabbing game",
            GrabbingHypothesisTest => "Test whatever the current hypothesis is for graph grabbing",
            BunkbedSiteCOunts => "Print percolation counts for bunkbed sites",
            BunkbedReducedConnectionCounts(k) => {
                str = format!("Print bunkbed connection counts for {}-gons", *k);
                &str
            }
            BunkbedReducedConnectionSimulation(reps, k) => {
                str = format!("Simulate bunkbed {}-connection counts {} times and print max ratios", k, reps);
                &str
            }
            BunkbedReducedConnectionDP(reps, k, edges) => {
                str = format!("Simulate bunkbed {}-connection counts with type-{} edges {} times and print max ratios using DP", k, edges, reps);
                &str
            }
            BunkbedCounterexampleSearch(_) => "Search for a counterexample to the bunkbed conjecture",
            PosetRelationProbabilities => "Print table of balance probabilities for a poset",
            PosetHasseDiagram => "Print Hasse diagram of poset",
            BalancedHeuristics => "Heuristics about whether the poset has balanced vertices",
            PosetPrintBalanceAsCap => "Print table of balance probabilities as a cap",
            PrintNorineHypercubeColourings => "Print info about good and bad Norine colourings",
            PrintSymmetricNorineHypercubeColourings(_, _) => "Print info about good and bad Norine symmetric colourings",
            PrintMinKernel => "Print a minimal 2-kernel in digraph",
            Unit => "Do nothing",
        };
        write!(f, "{}", name)
    }
}
