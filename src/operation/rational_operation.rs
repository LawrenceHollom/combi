use std::fmt;

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
                match text {
                    "domination_redundancy" | "gamma_red" => Some(DominationRedundancy),
                    "grabbing_coleaf_difference" | "gcwd" => Some(GrabbingColeafWeightedDifference),
                    &_ => IntOperation::of_string_result(text).map(OfInt),
                }
            }
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
        };
        write!(f, "{}", name)
    }
}