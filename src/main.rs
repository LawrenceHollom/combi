use std::io;
use std::time::*;

mod controller;
mod graph;
mod operator;
mod operation;
mod constructor;

use controller::*;

fn main() {
    println!("Enter instruction:");
    let mut text = String::new();
    io::stdin().read_line(&mut text).expect("Failed to read line");
    let start_time = SystemTime::now();
    let instruction = Instruction::of_string(&text);
    println!("Instruction constructed! {},\nTime: {}", instruction, start_time.elapsed().unwrap().as_millis());

    instruction.execute();
}
