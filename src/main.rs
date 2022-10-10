use std::io;

mod instruction;
mod graph;
mod operator;

use instruction::*;
use graph::Graph;

fn main() {
    println!("Enter instruction:");
    let mut text = String::new();
    io::stdin().read_line(&mut text).expect("Failed to read line");
    let instruction = Instruction::of_string(&text);
    println!("Instruction constructed! {}", instruction);
    let g = Graph::new(&instruction.constructor);
    //println!("The graph: ");
    //g.print();
    let numbers: Vec<String> = instruction.operations
            .iter()
            .map(|op| operator::operate(&g, op))
            .collect();
    println!("{}: [{}]", instruction.operations_string(), numbers.join(", "));
}
