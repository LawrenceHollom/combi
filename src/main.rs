use std::io;
use std::time::*;
use std::env;

mod controller;
mod graph;
mod dossier;
mod operation;
mod constructor;
mod pattern;
mod annotations;
mod digraph;
//mod poset;

use controller::*;

fn execute_instruction(text: &String) {
    let start_time = SystemTime::now();
    let instruction = Instruction::of_string(text);
    println!("Instruction constructed!\n{},\nTime: {}", instruction, start_time.elapsed().unwrap().as_millis());

    instruction.execute();
    println!("Finished! Time (s): {}", start_time.elapsed().unwrap().as_secs())
}

fn main() {
    env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        println!("Argument received, running directly!");
        execute_instruction(&args[1])
    } else {
        loop {
            println!("Enter instruction:");
            let mut text = String::new();
            io::stdin().read_line(&mut text).expect("Failed to read line");
            execute_instruction(&text)
        }
    }
}