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
    Debug,
}

impl NumToBoolInfix {
    pub fn of_string_result(text: &str) -> Option<NumToBoolInfix> {
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
    pub fn of_string_result(text: &str) -> Option<BoolToBoolInfix> {
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
    pub fn of_string_result(text: &str) -> Option<BoolOperation> {
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
                    BoolOperation::of_string_result(par1).and_then(|op1| 
                        BoolOperation::of_string_result(par2).map(|op2|
                            BoolInfix(infix, Box::new(op1), Box::new(op2))))
                } else {
                    None
                }
            }
            None => {
                let (func, args) = parse_function_like(text);
                match func.trim().to_lowercase().as_str() {
                    "not" | "!" | "¬" => {
                        BoolOperation::of_string_result(args[0]).map(|op|
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
                    "debug" => Some(Debug),
                    &_ => None,
                }
            }
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
            HasRegularLosslessEdgeDominator => "Has regular lossless edge-dominating set".to_owned(),
            IsKConnected(k) => format!("Is {}-connectd", *k),
            IsTriangleFree => "Is triangle free".to_owned(),
            CanBeEdgePartitionedIntoLinearForestAndMatching => {
                "Can G be edge-partitioned into a linear forest and a matching".to_owned()
            }
            GoodCubicDominationBase => "Some very specific thing for domination of cubic graphs.".to_owned(),
            Debug => "Returns true if some debugging tests trip".to_owned(),
        };
        write!(f, "{}", name)
    }
}