use std::fs;

use crate::graph::*;

pub fn new_graph(filename: &String) -> Graph {
    let path = format!("/mhome/damtp/q/lh569/Documents/rust/combi/manual/{}.gph", filename);
    match fs::read_to_string(path) {
        Ok(contents) => {
            let lines = contents.trim().lines().collect::<Vec<&str>>();
            let n: usize = lines[0].parse().unwrap();
            let mut adj_list: Vec<Vec<usize>> = vec![vec![]; n];

            for (i, line) in lines.iter().skip(1).enumerate() {
                for par in line.split(",") {
                    adj_list[i].push(par.parse().unwrap());
                }
            }

            Graph::of_adj_list(adj_list, File(filename.to_owned()))
        },
        Err(_e) => panic!("Cannot find graph constructor {}", filename)
    }

}   