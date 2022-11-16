use std::fmt;

pub mod bool_operation;
pub mod float_operation;
pub mod int_operation;
pub mod unit_operation;

use bool_operation::*;
use float_operation::*;
use int_operation::*;
use unit_operation::*;

pub enum Operation {
    Int(IntOperation),
    Bool(BoolOperation),
    Float(FloatOperation),
    Unit(UnitOperation),
}

impl Operation {
    pub fn of_string(text: &str) -> Operation {
        IntOperation::of_string_result(text).map(|x| Operation::Int(x))
            .or(BoolOperation::of_string_result(text).map(|x| Operation::Bool(x)))
            .or(FloatOperation::of_string_result(text).map(|x| Operation::Float(x)))
            .or(UnitOperation::of_string_result(text).map(|x| Operation::Unit(x)))
            .unwrap()
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operation::*;
        match self {
            Int(op) => write!(f, "{}", op),
            Bool(op) => write!(f, "{}", op),
            Float(op) => write!(f, "{}", op),
            Unit(op) => write!(f, "{}", op),
        }
    }
}