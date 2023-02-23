use std::io;
use std::time::*;
use std::env;

mod controller;
mod graph;
mod operator;
mod operation;
mod constructor;
mod pattern;

use controller::*;

fn main() {
    env::set_var("RUST_BACKTRACE", "1");
    loop {
        println!("Enter instruction:");
        let mut text = String::new();
        io::stdin().read_line(&mut text).expect("Failed to read line");
        let start_time = SystemTime::now();
        let instruction = Instruction::of_string(&text);
        println!("Instruction constructed!\n{},\nTime: {}", instruction, start_time.elapsed().unwrap().as_millis());

        instruction.execute();
        println!("Finished! Time (s): {}", start_time.elapsed().unwrap().as_secs())
    }
}