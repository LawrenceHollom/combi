use std::fmt;
use utilities::*;

use crate::operation::int_operation::*;

#[derive(Copy, Clone)]
pub enum IntToBoolInfix {
    More,
    Less,
    NotMore,
    NotLess,
    Equal
}

#[derive(Copy, Clone)]
pub enum BoolToBoolInfix {
    And,
    Or
}

#[derive(Clone)]
pub enum BoolOperation {
    IntInfix(IntToBoolInfix, IntOperation, IntOperation),
    BoolInfix(BoolToBoolInfix, Box<BoolOperation>, Box<BoolOperation>),
    IsConnected,
    HasLongMonotone,
}

impl IntToBoolInfix {
    pub fn of_string_result(text: &str) -> Option<IntToBoolInfix> {
        use IntToBoolInfix::*;
        match text {
            ">=" => Some(NotLess),
            "<=" => Some(NotMore),
            "<" => Some(Less),
            ">" => Some(More),
            "==" => Some(Equal),
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
                let int_to_bool_infix = IntToBoolInfix::of_string_result(op);
                let bool_infix = BoolToBoolInfix::of_string_result(op);

                if int_to_bool_infix.is_some() {
                    IntOperation::of_string_result(par1).map(|op1| 
                        IntOperation::of_string_result(par2).map(|op2|
                            IntInfix(int_to_bool_infix.unwrap(), op1, op2))).flatten()
                } else if bool_infix.is_some() {
                    BoolOperation::of_string_result(par1).map(|op1| 
                        BoolOperation::of_string_result(par2).map(|op2|
                            BoolInfix(bool_infix.unwrap(), Box::new(op1), Box::new(op2)))).flatten()
                } else {
                    None
                }
            }
            None => {
                match text.trim().to_lowercase().as_str() {
                    "is_connected" | "connected" => Some(IsConnected),
                    "has_long_monotone" | "has_monot" | "is_monot" => Some(HasLongMonotone),
                    &_ => None,
                }
            }
        }
    }
}

impl fmt::Display for IntToBoolInfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IntToBoolInfix::*;
        let str = match self {
            More => ">",
            Less => "<",
            NotMore => "<=",
            NotLess => ">=",
            Equal => "==",
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
        };
        write!(f, "{}", str)
    }
}

impl fmt::Display for BoolOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BoolOperation::*;
        let name = match self {
            IntInfix(infix, op1, op2) => 
                format!("Is ({}) {} ({})", *op1, *infix, *op2),
            BoolInfix(infix, op1, op2) =>
                format!("({}) {} ({})", *op1, *infix, *op2),
            IsConnected => "Is connected".to_owned(),
            HasLongMonotone => "Has 1->n monotone path".to_owned()
        };
        write!(f, "{}", name)
    }
}