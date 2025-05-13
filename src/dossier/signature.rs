use crate::dossier::Dossier;
use crate::entity::{graph::*, poset::*, *};
use crate::operation::bool_operation::{BoolOperation::*, *};
use crate::operation::int_operation::{IntOperation::*, *};

fn print_general_signature(
    mut dossier: Dossier,
    int_operations: Vec<IntOperation>,
    bool_operations: Vec<BoolOperation>,
) {
    let mut properties: Vec<(String, String)> = vec![
        ("SIGNATURE".to_owned(), "VALUE".to_owned()),
        ("~~~~".to_owned(), "~~~~".to_owned()),
    ];
    let mut ann = crate::dossier::AnnotationsBox::new();

    for op in int_operations.iter() {
        properties.push((
            format!("{}", op),
            format!("{}", dossier.operate_int(&mut ann, op)),
        ));
    }
    for op in bool_operations.iter() {
        properties.push((
            format!("{}", op),
            format!("{}", dossier.operate_bool(&mut ann, op)),
        ));
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

fn print_graph_signature(g: &Graph) {
    let is_big = g.n.at_least(25);
    let int_operations = if is_big {
        vec![
            Order,
            Size,
            NumComponents,
            MinDegree,
            MaxDegree,
            Diameter,
            Radius,
        ]
    } else {
        vec![
            Order,
            Size,
            NumComponents,
            MinDegree,
            MaxDegree,
            DominationNumber,
            ChromaticNumber,
            Girth,
            Connectedness,
            Diameter,
            Radius,
            Degeneracy,
        ]
    };
    let bool_operations = if is_big {
        vec![IsTriangleFree]
    } else {
        vec![IsPlanar, IsTriangleFree]
    };
    let dossier = Dossier::new(Entity::Graph(g.to_owned()));
    print_general_signature(dossier, int_operations, bool_operations)
}

fn print_poset_signature(p: &Poset) {
    let int_operations = vec![Order, Height, Width, Girth];
    let bool_operations = vec![];
    let dossier = Dossier::new(Entity::Poset(p.to_owned()));
    print_general_signature(dossier, int_operations, bool_operations)
}

pub fn print_signature(e: &Entity) {
    use Entity::*;
    match e {
        Graph(g) => print_graph_signature(g),
        Poset(p) => print_poset_signature(p),
        Digraph(_d) => todo!("If you want digraph signature, then you must code it."),
    }
}
