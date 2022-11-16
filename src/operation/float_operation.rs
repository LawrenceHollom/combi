use std::fmt;

use crate::operation::bool_operation::*;
use crate::operation::int_operation::*;

#[derive(Clone)]
pub enum FloatOperation {
    OfInt(IntOperation),
    OfBool(BoolOperation),
    Ratio(IntOperation, IntOperation),
}

impl FloatOperation {
    pub fn of_string_result(text: &str) -> Option<FloatOperation> {
        if text.contains("/") {
            let pars: Vec<&str> = text.split("/").map(|x| x.trim()).collect();
            if pars.len() == 2 {
                IntOperation::of_string_result(pars[0]).map(|op1| 
                    IntOperation::of_string_result(pars[1]).map(|op2|
                        FloatOperation::Ratio(op1, op2)
                )).flatten()
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl fmt::Display for FloatOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use FloatOperation::*;
        let name = match self {
            OfBool(op) => format!("Of Bool ({})", op),
            OfInt(op) => format!("Of int ({})", op),
            Ratio(op1, op2) => format!("Ratio ({}) / ({})", op1, op2),
        };
        write!(f, "{}", name)
    }
}