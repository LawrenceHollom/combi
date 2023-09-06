use std::fs;

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
            let mut adj: VertexVec<VertexVec<bool>> = VertexVec::new(order, &VertexVec::new(order, &false));

            for (i, line) in lines.iter().skip(1).enumerate() {
                if line.contains('~') {
                    let pars: Vec<&str> = line.split('~').collect();
                    let u: Vertex = Vertex::of_string(pars[0]);
                    for v_str in pars[1].split(',') {
                        let v: Vertex = Vertex::of_string(v_str);
                        adj[u][v] = true;
                        adj[v][u] = true;
                    }
                } else {
                    for par in line.split(',') {
                        let u = Vertex::of_usize(i);
                        let v = Vertex::of_string(par);
                        adj[u][v] = true;
                        adj[v][u] = true;
                    }
                }
            }

            let g = Graph::of_matrix(adj, Constructor::File(filename.to_owned()));
            if !g.is_adj_commutative() {
                panic!("Adjacency matrix of g is not commutative!");
            }
            g
        },
        Err(_e) => panic!("Cannot find graph constructor {}", filename)
    }

}   