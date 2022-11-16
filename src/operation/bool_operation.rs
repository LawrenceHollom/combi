use std::fmt;
use utilities::*;

use crate::operation::int_operation::*;

use super::float_operation::FloatOperation;

#[derive(Copy, Clone)]
pub enum NumToBoolInfix {
    More,
    Less,
    NotMore,
    NotLess,
    Equal
}

#[derive(Copy, Clone)]
pub enum BoolToBoolInfix {
    And,
    Or,
    Implies,
    Xor,
}

#[derive(Clone)]
pub enum BoolOperation {
    IntInfix(NumToBoolInfix, IntOperation, IntOperation),
    FloatInfix(NumToBoolInfix, FloatOperation, FloatOperation),
    BoolInfix(BoolToBoolInfix, Box<BoolOperation>, Box<BoolOperation>),
    IsConnected,
    HasLongMonotone,
}

impl NumToBoolInfix {
    pub fn of_string_result(text: &str) -> Option<NumToBoolInfix> {
        use NumToBoolInfix::*;
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
                        None => FloatOperation::of_string_result(par1).and_then(|op1| 
                                    FloatOperation::of_string_result(par2).map(|op2|
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
                match text.trim().to_lowercase().as_str() {
                    "is_connected" | "connected" => Some(IsConnected),
                    "has_long_monotone" | "has_monot" | "is_monot" => Some(HasLongMonotone),
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
            IntInfix(infix, op1, op2) => 
                format!("Is ({}) {} ({})", *op1, *infix, *op2),
            FloatInfix(infix, op1, op2) => 
                format!("Is ({}) {} ({})", *op1, *infix, *op2),
            BoolInfix(infix, op1, op2) =>
                format!("({}) {} ({})", *op1, *infix, *op2),
            IsConnected => "Is connected".to_owned(),
            HasLongMonotone => "Has 1->n monotone path".to_owned()
        };
        write!(f, "{}", name)
    }
}