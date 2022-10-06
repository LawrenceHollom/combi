use std::io;

mod instruction;

use instruction::*;

fn main() {
    println!("Enter instruction:");
    let mut text = String::new();
    io::stdin().read_line(&mut text).expect("Failed to read line");
    let instruction = Instruction::of_string(&text);
    println!("Instruction constructed! {}", instruction.to_string());
}
