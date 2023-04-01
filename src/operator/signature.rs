use crate::{graph::*, operator::Operator};

pub fn print_signature(g: &Graph) {
    let mut properties: Vec<(String, String)> = vec![];
    use crate::operation::int_operation::IntOperation::*;
    let int_operations = 
        vec![Order, Size, NumComponents, MinDegree, MaxDegree, 
        DominationNumber, ChromaticNumber, Girth, Connectedness,
        Diameter, Radius];
    use crate::operation::bool_operation::BoolOperation::*;
    let bool_operations =
        vec![IsPlanar, IsTriangleFree];
    let mut ann = crate::operator::AnnotationsBox::new();
    let mut operator = Operator::new(g.to_owned());
    for op in int_operations.iter() {
        properties.push((format!("{}", op), format!("{}", operator.operate_int(&mut ann, op))));
    }
    for op in bool_operations.iter() {
        properties.push((format!("{}", op), format!("{}", operator.operate_bool(&mut ann, op))));
    }
    let max_len = properties.iter().map(|(s, _)| s.len()).max().unwrap();
    for (s1, s2) in properties.iter() {
        let pad_len = max_len - s1.len();
        for _i in 0..(pad_len + 1) {
            print!(" ");
        }
        println!("{} : {}", *s1, *s2);
    }
    println!();
}