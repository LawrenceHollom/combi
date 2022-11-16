use std::fmt;

#[derive(Copy, Clone)]
pub enum UnitOperation {
    Print,
    RawBunkbed,
    BunkbedPosts,
    BunkbedSimulation,
    Unit,
}

impl UnitOperation {
    pub fn of_string_result(text: &str) -> Option<UnitOperation> {
        use UnitOperation::*;
        match text.trim().to_lowercase().as_str() {
            "print" => Some(Print),
            "bunkbed" => Some(RawBunkbed),
            "bunkbed_posts" | "posts" => Some(BunkbedPosts),
            "bunkbed_sim" => Some(BunkbedSimulation),
            "()" => Some(Unit),
            &_ => None,
        }
    }
}

impl fmt::Display for UnitOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnitOperation::*;
        let name = match self {
            Print => "Print",
            RawBunkbed => "Raw bunkbed",
            BunkbedPosts => "Bunkbed posts",
            BunkbedSimulation => "Bunkbed simulation",
            Unit => "Do nothing",
        };
        write!(f, "{}", name)
    }
}