use std::fmt;

pub mod bool_operation;
pub mod rational_operation;
pub mod int_operation;
pub mod unit_operation;

use bool_operation::*;
use rational_operation::*;
use int_operation::*;
use unit_operation::*;

pub enum Operation {
    Int(IntOperation),
    Bool(BoolOperation),
    Rational(RationalOperation),
    Unit(UnitOperation),
}

impl Operation {
    pub fn of_string(text: &str) -> Operation {
        IntOperation::of_string_result(text).map(Operation::Int)
            .or_else(|| BoolOperation::of_string_result(text).map(Operation::Bool))
            .or_else(|| RationalOperation::of_string_result(text).map(Operation::Rational))
            .or_else(|| UnitOperation::of_string_result(text).map(Operation::Unit))
            .unwrap_or_else(|| panic!("Unknown operation {}", text))
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operation::*;
        match self {
            Int(op) => write!(f, "{}", op),
            Bool(op) => write!(f, "{}", op),
            Rational(op) => write!(f, "{}", op),
            Unit(op) => write!(f, "{}", op),
        }
    }
}