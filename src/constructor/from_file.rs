use std::fs;

use crate::graph::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new_graph(filename: &String) -> Graph {
    let mut pathbuf = std::env::current_exe().unwrap();
    pathbuf.pop();
    while !pathbuf.ends_with("combi/") {
        pathbuf.pop();
    }
    pathbuf.push(format!("manual/{}.gph", filename));
    match fs::read_to_string(pathbuf) {
        Ok(contents) => {
            let lines = contents.trim().lines().collect::<Vec<&str>>();
            let n: usize = lines[0].parse().unwrap();
            let order = Order::of_usize(n);
            let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);

            if lines[1].contains("~") {
                for line in lines.iter().skip(1) {
                    let pars: Vec<&str> = line.split("~").collect();
                    let u: Vertex = Vertex::of_string(pars[0]);
                    let v: Vertex = Vertex::of_string(pars[1]);
                    adj_list[u].push(v);
                    adj_list[v].push(u);
                }
            } else {
                for (i, line) in lines.iter().skip(1).enumerate() {
                    for par in line.split(",") {
                        adj_list[Vertex::of_usize(i)].push(Vertex::of_string(par));
                    }
                }
            }

            let g = Graph::of_adj_list(adj_list, Constructor::File(filename.to_owned()));
            if !g.is_adj_commutative() {
                panic!("Adjacency matrix of g is not commutative!");
            }
            g
        },
        Err(_e) => panic!("Cannot find graph constructor {}", filename)
    }

}   