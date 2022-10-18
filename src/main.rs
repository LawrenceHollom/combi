use std::io;
use std::time::*;

mod instruction;
mod graph;
mod operator;

use instruction::*;
use operator::Operator;
use graph::Graph;

fn main() {
    println!("Enter instruction:");
    let mut text = String::new();
    io::stdin().read_line(&mut text).expect("Failed to read line");
    let start_time = SystemTime::now();
    let instruction = Instruction::of_string(&text);
    println!("Instruction constructed! {},\nTime: {}", instruction, start_time.elapsed().unwrap().as_millis());

    for _ in 0..instruction.repeats {
        let g = Graph::new(&instruction.constructor);
        let rep_start = SystemTime::now();
        
        let mut operator = Operator::new();
        let numbers: Vec<String> = instruction.operations
                .iter()
                .map(|op| operator.operate(&g, op))
                .collect();
        println!("{}: [{}], time: {}", instruction.operations_string(), numbers.join(", "),
                rep_start.elapsed().unwrap().as_millis());
    }
}
