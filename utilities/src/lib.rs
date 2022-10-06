pub struct Order(pub u32);
pub struct Degree(pub u32);

impl Order {
    pub fn of_string(text: &str) -> Order {
        Order(text.parse().unwrap())
    }
}

impl Degree {
    pub fn of_string(text: &str) -> Degree {
        Degree(text.parse().unwrap())
    }
}