use std::fmt;

use utilities::parse_function_like;

#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug, PartialOrd, Ord)]
pub enum StringListOperation {
    BunkbedSiteSignatures,
    Serialise,
}

impl StringListOperation {
    pub fn of_string_result(text: &str) -> Option<Self> {
        use StringListOperation::*;
        let (func, _args) = parse_function_like(text);
        match func.trim().to_lowercase().as_str() {
            "bbss" => Some(BunkbedSiteSignatures),
            "serialise" | "code" => Some(Serialise),
            &_ => None,
        }
    }
}

impl fmt::Display for StringListOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StringListOperation::*;
        let name = match self {
            BunkbedSiteSignatures => "Bunkbed site reachability signatures",
            Serialise => "Serialise graph to a string",
        };
        write!(f, "{}", name)
    }
}
