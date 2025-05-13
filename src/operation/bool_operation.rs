use std::fmt;
use utilities::*;

use crate::operation::int_operation::*;

use super::rational_operation::RationalOperation;

#[derive(Eq, Hash, PartialEq, Copy, Clone, Debug, PartialOrd, Ord)]
pub enum NumToBoolInfix {
    More,
    Less,
    NotMore,
    NotLess,
    Equal,
    NotEqual,
}

#[derive(Eq, Hash, PartialEq, Copy, Clone, Debug, PartialOrd, Ord)]
pub enum BoolToBoolInfix {
    And,
    Or,
    Implies,
    Xor,
}

#[derive(Eq, Hash, PartialEq, Clone, Debug, PartialOrd, Ord)]
pub enum BoolOperation {
    IntInfix(NumToBoolInfix, IntOperation, IntOperation),
    FloatInfix(NumToBoolInfix, RationalOperation, RationalOperation),
    BoolInfix(BoolToBoolInfix, Box<BoolOperation>, Box<BoolOperation>),
    Not(Box<BoolOperation>),
    Const(bool),
    IsConnected,
    HasLongMonotone,
    HasIntervalColoring,
    IsPlanar,
    IsRegular,
    BunkbedDiffsAllUnimodal,
    HasRegularLosslessEdgeDominator,
    IsDominationNumberAtLeast(usize),
    IsKConnected(usize),
    IsTriangleFree,
    CanBeEdgePartitionedIntoLinearForestAndMatching,
    GoodCubicDominationBase, // This one is incredibly specific.
    GameChromaticWinner(usize),
    IsChromaticGameMonotone,
    IsChromaticGameStronglyMonotone,
    ArboricityGameWinner(usize),
    IsDDegenerate(Degree),
    IsBunkbedPostRemovalInductionGood,
    CanChromaticGameHaveDudUniqueWin,
    GameChromaticWinnerWithDuds(usize),
    ChromaticGameSubsetMonotonicity(usize),
    LinearGameChromaticWinner(usize),
    GameGrundyWinner(usize),
    MakerWinsConnectedChromaticGame(usize),
    IsBipartite,
    MakerWinsChromaticGameGreedily(usize),
    HasSeymourVertex,
    CanBobWinGraphGrabbing(Option<usize>),
    IsOddCoronaFree,
    IsOddSemicoronaFree,
    IsForkFree,
    BunkbedSiteHasBadConditioning,
    ContradictsBunkbedSiteConjecture,
    ContradictsReducedBunkbedConjecture,
    ApproxContradictsReducedBunkbedConjecture(usize),
    ContradictsReducedConditionedBunkbedConjecture,
    IsBunkbedReducible,
    BunkbedGadget,
    IsPosetIncomparabilityConnected,
    HasTwinElements,
    HasAlmostTwinElements,
    HasAlmostTwinElementsIgnoreMaximal,
    NumLinearExtensionsLessThan(usize),
    IsHeuristicallyBalanced,
    HasMaximalN,
    IsForest,
    HasSource,
    HasSink,
    HasBasin,
    HasKernelOfSizeAtMost(usize),
    KuramotoSynchronises(usize),
    KuramotoSimplest(usize),
    KuramotoCyclic,
    IsKozmaNitzanFalse,
    IsSiteKozmaNitzanFalse,
    IsEveryEdgeWitnessed,
    HasBidirectionalEdge,
    HasFourCycle,
    IsGenericallyHyperbolicEmbeddable(bool),
    HasFewEdgesHyperbolic,
    Debug,
}

impl NumToBoolInfix {
    pub fn of_string_result(text: &str) -> Option<Self> {
        use NumToBoolInfix::*;
        match text {
            ">=" => Some(NotLess),
            "<=" => Some(NotMore),
            "<" => Some(Less),
            ">" => Some(More),
            "=" | "==" => Some(Equal),
            "<>" | "!=" => Some(NotEqual),
            &_ => None
        }
    }
}

impl BoolToBoolInfix {
    pub fn of_string_result(text: &str) -> Option<Self> {
        use BoolToBoolInfix::*;
        match text {
            "&" | "&&" => Some(And),
            "|" | "||" => Some(Or),
            "=>" | "->" => Some(Implies),
            "^" => Some(Xor),
            &_ => None
        }
    }
}

impl BoolOperation {
    pub fn of_string_result(text: &str) -> Option<Self> {
        use BoolOperation::*;
        match parse_infix_like(text) {
            Some((par1, op, par2)) => {
                println!("Infix like! [{}] [{}] [{}]", par1, op, par2);
                let num_to_bool_infix = NumToBoolInfix::of_string_result(op);
                let bool_infix = BoolToBoolInfix::of_string_result(op);

                if let Some(infix) = num_to_bool_infix {
                    //writing readable code is my passion
                    match IntOperation::of_string_result(par1).and_then(|op1| 
                        IntOperation::of_string_result(par2).map(|op2|
                            IntInfix(infix, op1, op2))) {
                        Some(op) => Some(op),
                        None => RationalOperation::of_string_result(par1).and_then(|op1| 
                                    RationalOperation::of_string_result(par2).map(|op2|
                                        FloatInfix(infix, op1, op2)))
                    }
                    
                } else if let Some(infix) = bool_infix {
                    Self::of_string_result(par1).and_then(|op1| 
                        Self::of_string_result(par2).map(|op2|
                            BoolInfix(infix, Box::new(op1), Box::new(op2))))
                } else {
                    None
                }
            }
            None => {
                let (func, args) = parse_function_like(text);
                println!("PARSING: func: {}, args: {:?}", func, args);
                match func.trim().to_lowercase().as_str() {
                    "not" | "!" | "¬" => {
                        Self::of_string_result(args[0]).map(|op|
                            Not(Box::new(op)))
                    }
                    "domination_number_at_least" | "gamma_at_least" => {
                        Some(IsDominationNumberAtLeast(args[0].parse().unwrap()))
                    }
                    "is_connected" | "connected" => Some(IsConnected),
                    "has_long_monotone" | "has_monot" | "is_monot" => Some(HasLongMonotone),
                    "has_interval_coloring" | "has_interval" => Some(HasIntervalColoring),
                    "is_planar" | "planar" => Some(IsPlanar),
                    "is_regular" | "regular" => Some(IsRegular),
                    "bunkbed_all_unimodal" => Some(BunkbedDiffsAllUnimodal),
                    "has_regular_lossless_edge_dominator" | "has_rled" => Some(HasRegularLosslessEdgeDominator),
                    "true" => Some(Const(true)),
                    "false" => Some(Const(false)),
                    "is_k_connected" | "k_connected" | "k_conn" => Some(IsKConnected(args[0].parse().unwrap())),
                    "triangle_free" | "tri_free" | "k3_free" => Some(IsTriangleFree),
                    "is_forest_and_matching" | "ifam" => Some(CanBeEdgePartitionedIntoLinearForestAndMatching),
                    "gcdb" => Some(GoodCubicDominationBase),
                    "does_alice_win_chromatic_game" | "a_wins_chi_g" | "chi_g_win" => {
                        Some(GameChromaticWinner(args[0].parse().unwrap()))
                    }
                    "chromatic_game_monotone" | "chi_g_monot" => Some(IsChromaticGameMonotone),
                    "chromatic_game_strongly_monotone" | "chi_g_sm" => Some(IsChromaticGameStronglyMonotone),
                    "arboricity_game_winner" | "a_g_win" => {
                        Some(ArboricityGameWinner(args[0].parse().unwrap()))
                    }
                    "is_d_degenerate" | "d_degen" => {
                        Some(IsDDegenerate(Degree::of_string(args[0])))
                    }
		            "is_bb_p_r_g" => Some(IsBunkbedPostRemovalInductionGood),
                    "can_dud_win" => Some(CanChromaticGameHaveDudUniqueWin),
                    "chi_g_duds" => Some(GameChromaticWinnerWithDuds(args[0].parse().unwrap())),
                    "chi_g_subsets" => Some(ChromaticGameSubsetMonotonicity(args[0].parse().unwrap())),
                    "linear_chromatic_winner" | "lin_chi_g_win" | "lcgw" => {
                        Some(LinearGameChromaticWinner(args[0].parse().unwrap()))
                    }
                    "game_grundy_winner" | "grundy_win" => {
                        Some(GameGrundyWinner(args[0].parse().unwrap()))
                    }
                    "chi_cg_win" => Some(MakerWinsConnectedChromaticGame(args[0].parse().unwrap())),
                    "is_bipartite" | "is_bip" => Some(IsBipartite),
                    "alice_greedy_win" => Some(MakerWinsChromaticGameGreedily(args[0].parse().unwrap())),
                    "seymour" => Some(HasSeymourVertex),
                    "bob_grabbing_win" | "bgw" => {
                        if args.is_empty() {
                            Some(CanBobWinGraphGrabbing(None))
                        } else {
                            Some(CanBobWinGraphGrabbing(Some(args[0].parse().unwrap())))
                        }
                    }
                    "odd_corona_free" => Some(IsOddCoronaFree),
                    "odd_semicorona_free" => Some(IsOddSemicoronaFree),
                    "fork_free" => Some(IsForkFree),
                    "bunkbed_sites_conditioning" | "bb_sites_cond" => Some(BunkbedSiteHasBadConditioning),
                    "contradicts_bb_sites" | "cbbs" => Some(ContradictsBunkbedSiteConjecture),
                    "contradicts_reduced_bb" | "crbb" => Some(ContradictsReducedBunkbedConjecture),
                    "acrbb" => Some(ApproxContradictsReducedBunkbedConjecture(args[0].parse().unwrap())),
                    "crcbb" => Some(ContradictsReducedConditionedBunkbedConjecture),
                    "is_bb_reducible" | "is_bbr" => Some(IsBunkbedReducible),
                    "bb_gadget" => Some(BunkbedGadget),
                    "incomp_connected" | "is_incomp_connected" => Some(IsPosetIncomparabilityConnected),
                    "has_twins" => Some(HasTwinElements),
                    "has_almost_twins" => Some(HasAlmostTwinElements),
                    "has_non_maximal_almost_twins" => Some(HasAlmostTwinElementsIgnoreMaximal),
                    "num_extensions_less_than" => Some(NumLinearExtensionsLessThan(args[0].parse().unwrap())),
                    "heuristically_balanced" => Some(IsHeuristicallyBalanced),
                    "has_maximal_n" => Some(HasMaximalN),
                    "is_forest" => Some(IsForest),
                    "has_source" => Some(HasSource),
                    "has_sink" => Some(HasSink),
                    "has_basin" => Some(HasBasin),
                    "has_kernel_at_most" => Some(HasKernelOfSizeAtMost(args[0].parse().unwrap())),
                    "kuramoto_syncs" => Some(KuramotoSynchronises(args[0].parse().unwrap())),
                    "kuramoto_simplest" => Some(KuramotoSimplest(args[0].parse().unwrap())),
                    "kuramoto_cyclic" => Some(KuramotoCyclic),
                    "kozma_nitzan" => Some(IsKozmaNitzanFalse),
                    "site_kozma_nitzan" => Some(IsSiteKozmaNitzanFalse),
                    "is_witnessed" => Some(IsEveryEdgeWitnessed),
                    "has_bidirectional" => Some(HasBidirectionalEdge),
                    "has_c4" => Some(HasFourCycle),
                    "embeds_h" => Some(IsGenericallyHyperbolicEmbeddable(args.first()?.parse().unwrap_or(false))),
                    "few_edges_h" => Some(HasFewEdgesHyperbolic),
                    "debug" => Some(Debug),
                    &_ => None,
                }
            }
        }
    }

    pub fn is_recursive(&self) -> bool {
        use BoolOperation::*;
        match self {
            IntInfix(..) | FloatInfix(..) | BoolInfix(..) | Not(..) | Const(..) => true,
            _ => false,
        }
    }
}

impl fmt::Display for NumToBoolInfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use NumToBoolInfix::*;
        let str = match self {
            More => ">",
            Less => "<",
            NotMore => "<=",
            NotLess => ">=",
            Equal => "==",
            NotEqual => "<>",
        };
        write!(f, "{}", str)
    }
}


impl fmt::Display for BoolToBoolInfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BoolToBoolInfix::*;
        let str = match self {
            And => "&&",
            Or => "||",
            Implies => "=>",
            Xor => "^",
        };
        write!(f, "{}", str)
    }
}

impl fmt::Display for BoolOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BoolOperation::*;
        let name = match self {
            IntInfix(infix, op1, op2) => {
                format!("Is ({}) {} ({})", *op1, *infix, *op2)
            }
            FloatInfix(infix, op1, op2) => {
                format!("Is ({}) {} ({})", *op1, *infix, *op2)
            }
            BoolInfix(infix, op1, op2) => {
                format!("({}) {} ({})", *op1, *infix, *op2)
            }
            IsDominationNumberAtLeast(lower_bound) => {
                format!("Is the domination number at least {}", lower_bound)
            }
            Not(op) => format!("Not ({})", *op),
            Const(val) => format!("Constant ({})", val),
            IsConnected => "Is connected".to_owned(),
            HasLongMonotone => "Has 1->n monotone path".to_owned(),
            HasIntervalColoring => "Has some interval coloring".to_owned(),
            IsPlanar => "Is planar".to_owned(),
            IsRegular => "Is regular".to_owned(),
            BunkbedDiffsAllUnimodal => "Bunkbed diff polys are all unimodal".to_owned(),
            HasRegularLosslessEdgeDominator => {
                "Has regular lossless edge-dominating set".to_owned()
            }
            IsKConnected(k) => format!("Is {}-connected", *k),
            IsTriangleFree => "Is triangle free".to_owned(),
            CanBeEdgePartitionedIntoLinearForestAndMatching => {
                "Can G be edge-partitioned into a linear forest and a matching".to_owned()
            }
            GoodCubicDominationBase => {
                "Some very specific thing for domination of cubic graphs.".to_owned()
            }
            GameChromaticWinner(k) => {
                format!("Whether Alice wins the chromatic game on {} colours", k)
            }
            IsChromaticGameMonotone => {
                "Is the chromatic game monotone in the number of colours".to_owned()
            }
            IsChromaticGameStronglyMonotone => {
                "Is the chromatic game strongly monotone".to_owned()
            }
            ArboricityGameWinner(k) => {
                format!("Whether Maker wins the arboricity game on {} colours", k)
            }
            IsDDegenerate(d) => {
                format!("Whether the graph is {}-degenerate", d)
            }
            IsBunkbedPostRemovalInductionGood => {
                "Is bunkbed post removal induction good?".to_string()
            }
            CanChromaticGameHaveDudUniqueWin => {
                "Whether chromatic game have dud as unique winning move".to_string()
            }
            GameChromaticWinnerWithDuds(k) => {
                format!("Whether Maker wins the chromatic game on {} colours (one of which is a dud)", k)
            }
            ChromaticGameSubsetMonotonicity(k) => {
                format!("Whether the chromatic game has subset-monotonicity from k = {} to {}", k, k + 1)
            }
            LinearGameChromaticWinner(k) => {
                format!("Whether Maker wins the linear chromatic game on {} colours", k)
            }
            GameGrundyWinner(k) => {
                format!("Whether Maker wins Grundy game on {} colours", k)
            }
            MakerWinsConnectedChromaticGame(k) => {
                format!("Whether Maker wins the connected chromatic game on {} colours", k)
            }
            IsBipartite => "Is bipartite".to_owned(),
            MakerWinsChromaticGameGreedily(k) => {
                format!("Whether Maker wins chromatic game with {} colours playing greedily", k)
            }
            HasSeymourVertex => "Has a Seymour vertex under some random orientations".to_owned(),
            CanBobWinGraphGrabbing(max_weight) => {
                format!("Can Bob with the graph grabbing game with max_weight {:?}", max_weight)
            }
            IsOddCoronaFree => "Has no induced corona product of an odd cycle and a point".to_owned(),
            IsOddSemicoronaFree => "Has no induced odd semicorona".to_owned(),
            IsForkFree => "Has no induced fork or 3-corona".to_owned(),
            BunkbedSiteHasBadConditioning => "Is bunkbed sites conditioning bad".to_owned(),
            ContradictsBunkbedSiteConjecture => "Does this contradict the bunkbed sites conjecture".to_owned(),
            ContradictsReducedBunkbedConjecture => "Does this contradict the reduced bunkbed conjecture".to_owned(),
            ApproxContradictsReducedBunkbedConjecture(samples) => {
                format!("Approx contradicts the reduced bunkbed conjecture with {} samples", samples)
            }
            ContradictsReducedConditionedBunkbedConjecture => "Does this contradict the reduced conditioned bunkbed conjecture".to_owned(),
            IsBunkbedReducible => "Is bunkbed reducible".to_owned(),
            BunkbedGadget => "Use as a gadget in the search for a bunkbed counterexample".to_owned(),
            IsPosetIncomparabilityConnected => "Has connected incomparability graph".to_owned(),
            HasTwinElements => "Has a pair of twin elements".to_owned(),
            HasAlmostTwinElements => "Has a pair of almost-twin elements".to_owned(),
            HasAlmostTwinElementsIgnoreMaximal => "Has non-maximal almost-twins".to_owned(),
            NumLinearExtensionsLessThan(k) => format!("Has at most {} linear extensions", k),
            IsHeuristicallyBalanced => "Is heuristically balanced".to_owned(),
            HasMaximalN => "Does this poset have a maximal N".to_owned(),
            IsForest => "Is a forest".to_owned(),
            HasSource => "Has a source vertex".to_owned(),
            HasSink => "Has a sink vertex".to_owned(),
            HasBasin => "Has a vertex to which everything can flow".to_owned(),
            HasKernelOfSizeAtMost(size) => format!("Has a 2-kernel of size at most {}", size),
            KuramotoSynchronises(k) => format!("Does Kuramoto synchronise under a random start ({} attempts)?", k),
            KuramotoSimplest(k) => format!("Does Kuramoto synchronise, and find simplest unsyncd ({} attempts)?", k),
            KuramotoCyclic => "Does Kuramoto synchronise from cyclic start".to_owned(),
            IsKozmaNitzanFalse => "Is the Kozma--Nitzan conjecture false".to_owned(),
            IsSiteKozmaNitzanFalse => "Is the site-percolation Kozma--Nitzan conjecture false".to_owned(),
            IsEveryEdgeWitnessed => "Is every edge witnessed (common parent)".to_owned(),
            HasBidirectionalEdge => "Has a bidirectional edge".to_owned(),
            HasFourCycle => "Contains a K4".to_owned(),
            IsGenericallyHyperbolicEmbeddable(_) => "Can be embedded as d-distance in hyperbolic plane".to_owned(),
            HasFewEdgesHyperbolic => "Has at most 2n-3 edges hereditarily".to_owned(),
            Debug => "Returns true if some debugging tests trip".to_owned(),
        };
        write!(f, "{}", name)
    }
}
