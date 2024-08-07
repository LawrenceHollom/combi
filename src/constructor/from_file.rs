use std::fs;

use super::*;
use utilities::vertex_tools::*;

fn new_graph(contents: String, filename: &String) -> Graph {
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
}

fn new_poset(contents: String, filename: &String) -> Poset {
    let lines = contents.trim().lines().collect::<Vec<&str>>();
    let n: usize = lines[0].parse().unwrap();
    let order = Order::of_usize(n);
    let mut gt: VertexVec<VertexVec<bool>> = VertexVec::new(order, &VertexVec::new(order, &false));

    fn close_transitively(gt: &mut VertexVec<VertexVec<bool>>, u: Vertex, v: Vertex, order: Order) {
        // Take the transitive closure.
        for x in order.iter_verts() {
            if gt[x][u] {
                gt[x][v] = true;
            }
            if gt[v][x] {
                gt[u][x] = true;
            }
        }
        if gt[v][u] {
            panic!("The transitive closure of the relation is not antisymmetric!")
        }
    }

    for line in lines.iter().skip(1) {
        if line.contains('>') {
            let pars: Vec<&str> = line.split('>').collect();
            let u: Vertex = Vertex::of_string(pars[0]);
            for v_str in pars[1].split(',') {
                let v: Vertex = Vertex::of_string(v_str);
                gt[u][v] = true;
                close_transitively(&mut gt, u, v, order);
            }
        } else {
            let pars: Vec<&str> = line.split('<').collect();
            let u: Vertex = Vertex::of_string(pars[0]);
            for v_str in pars[1].split(',') {
                let v: Vertex = Vertex::of_string(v_str);
                gt[v][u] = true;
                close_transitively(&mut gt, v, u, order);
            }
        }
    }

    Poset::of_ordering(gt, Constructor::File(filename.to_owned()))
}

pub fn new_entity(filename: &String) -> Entity {
    let mut pathbuf = std::env::current_exe().unwrap();
    pathbuf.pop();
    while !pathbuf.ends_with("combi/") {
        pathbuf.pop();
    }

    let mut graph_pathbuf = pathbuf.to_owned();
    graph_pathbuf.push(format!("manual/{}.gph", filename));
    let mut poset_pathbuf = pathbuf.to_owned();
    poset_pathbuf.push(format!("manual/{}.pst", filename));

    if graph_pathbuf.exists() {
        match fs::read_to_string(graph_pathbuf) {
            Ok(contents) => Entity::Graph(new_graph(contents, filename)),
            Err(e) => panic!("Cannot find graph constructor {} (Error: {})", filename, e),
        }
    } else if poset_pathbuf.exists() {
        match fs::read_to_string(poset_pathbuf) {
            Ok(contents) => Entity::Poset(new_poset(contents, filename)),
            Err(e) => panic!("Cannot find graph constructor {} (Error: {})", filename, e),
        }
    } else {
        panic!("Could not find constructor {}", filename)
    }

}