use std::fs;

use crate::graph::*;

pub fn new_graph(filename: &String) -> Graph {
    let mut pathbuf = std::env::current_exe().unwrap();
    pathbuf.pop();
    pathbuf.pop();
    pathbuf.pop();
    pathbuf.push(format!("manual/{}.gph", filename));
    match fs::read_to_string(pathbuf) {
        Ok(contents) => {
            let lines = contents.trim().lines().collect::<Vec<&str>>();
            let n: usize = lines[0].parse().unwrap();
            let mut adj_list: Vec<Vec<usize>> = vec![vec![]; n];

            if lines[1].contains("~") {
                for line in lines.iter().skip(1) {
                    let pars: Vec<&str> = line.split("~").collect();
                    let u: usize = pars[0].trim().parse().unwrap();
                    let v: usize = pars[1].trim().parse().unwrap();
                    adj_list[u].push(v);
                    adj_list[v].push(u);
                }
            } else {
                for (i, line) in lines.iter().skip(1).enumerate() {
                    for par in line.split(",") {
                        adj_list[i].push(par.parse().unwrap());
                    }
                }
            }

            Graph::of_adj_list(adj_list, File(filename.to_owned()))
        },
        Err(_e) => panic!("Cannot find graph constructor {}", filename)
    }

}   