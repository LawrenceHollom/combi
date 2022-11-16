use std::fmt;

use utilities::parse_infix_like;

use crate::operation::bool_operation::*;
use crate::operation::int_operation::*;

#[derive(Clone, Copy)]
pub enum ArithmeticOperation {
    Sum,
    Difference,
    Product,
    Ratio,
}

#[derive(Clone)]
pub enum FloatOperation {
    OfInt(IntOperation),
    OfBool(Box<BoolOperation>),
    Arithmetic(ArithmeticOperation, Box<FloatOperation>, Box<FloatOperation>),
}

impl FloatOperation {
    fn wrap_arithmetic(op: ArithmeticOperation, par1: &str, par2: &str) -> Option<FloatOperation> {
        Self::of_string_result(par1).and_then(|par1| 
            Self::of_string_result(par2).map(|par2|
                FloatOperation::Arithmetic(op, Box::new(par1), Box::new(par2))
        ))
    }

    pub fn of_string_result(text: &str) -> Option<FloatOperation> {
        match parse_infix_like(text) {
            Some((par1, op, par2)) => {
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
            None => IntOperation::of_string_result(text).map(FloatOperation::OfInt),
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

impl fmt::Display for FloatOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use FloatOperation::*;
        let name = match self {
            OfBool(op) => format!("Of Bool ({})", op),
            OfInt(op) => format!("Of int ({})", op),
            Arithmetic(arith, op1, op2) => format!("({}) {} ({})", *op1, arith, *op2),
        };
        write!(f, "{}", name)
    }
}