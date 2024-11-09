use std::fmt;

use utilities::parse_function_like;
use utilities::parse_infix_like;

use crate::operation::bool_operation::*;
use crate::operation::int_operation::*;

#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug, PartialOrd, Ord)]
pub enum ArithmeticOperation {
    Sum,
    Difference,
    Product,
    Ratio,
}

#[derive(Eq, Hash, PartialEq, Clone, Debug, PartialOrd, Ord)]
pub enum RationalOperation {
    OfInt(IntOperation),
    OfBool(Box<BoolOperation>),
    Arithmetic(ArithmeticOperation, Box<RationalOperation>, Box<RationalOperation>),
    DominationRedundancy,
    GrabbingColeafWeightedDifference,
    PosetBalance,
    PosetCapBalance,
    PosetBalanceWithMinimal,
    NorineAverageDistance(usize),
}

impl RationalOperation {
    fn wrap_arithmetic(op: ArithmeticOperation, par1: &str, par2: &str) -> Option<RationalOperation> {
        Self::of_string_result(par1).and_then(|par1| 
            Self::of_string_result(par2).map(|par2|
                RationalOperation::Arithmetic(op, Box::new(par1), Box::new(par2))
        ))
    }

    pub fn of_string_result(text: &str) -> Option<RationalOperation> {
        match parse_infix_like(text) {
            Some((par1, op, par2)) => {
                println!("DEBUG: INFIX LIKE: [{}] [{}] [{}]", par1, op, par2);
                use ArithmeticOperation::*;
                let arith = match op {
                    "+" => Some(Sum),
                    "-" => Some(Difference),
                    "*" => Some(Product),
                    "/" => Some(Ratio),
                    &_ => None,
                };
                arith.and_then(|op| Self::wrap_arithmetic(op, par1, par2))
            }
            None => {
                use RationalOperation::*;
                let (func, args) = parse_function_like(text);
                match func.trim().trim_end_matches(')').to_lowercase().as_str() {
                    "domination_redundancy" | "gamma_red" => Some(DominationRedundancy),
                    "grabbing_coleaf_difference" | "gcwd" => Some(GrabbingColeafWeightedDifference),
                    "balance" => Some(PosetBalance),
                    "poset_cap_balance" => Some(PosetCapBalance),
                    "balance_with_min" => Some(PosetBalanceWithMinimal),
                    "avg_norine" => Some(NorineAverageDistance(args[0].parse().unwrap_or(0))),
                    &_ => IntOperation::of_string_result(text).map(OfInt),
                }
            }
        }
    }

    pub fn is_recursive(&self) -> bool {
        use RationalOperation::*;
        match self {
            OfInt(_) | OfBool(_) | Arithmetic(..) => true,
            _ => false,
        }
    }
}

impl fmt::Display for ArithmeticOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ArithmeticOperation::*;
        let str = match self {
            Sum => "+",
            Difference => "-",
            Product => "*",
            Ratio => "/",
        };
        write!(f, "{}", str)
    }
}

impl fmt::Display for RationalOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RationalOperation::*;
        let name = match self {
            OfBool(op) => format!("Of Bool ({})", op),
            OfInt(op) => format!("{}", op),
            Arithmetic(arith, op1, op2) => format!("({}) {} ({})", *op1, arith, *op2),
            DominationRedundancy => "Domination redundancy ratio".to_owned(),
            GrabbingColeafWeightedDifference => "Grabbing game score diff with coleaf weights".to_owned(),
            PosetBalance => "Balance constant of the poset".to_owned(),
            PosetCapBalance => "Balance constant as the cap of a ladder".to_owned(),
            PosetBalanceWithMinimal => "Balance among pairs including a min".to_owned(),
            NorineAverageDistance(_) => "Average Norine distance between antipodes".to_owned(),
        };
        write!(f, "{}", name)
    }
}