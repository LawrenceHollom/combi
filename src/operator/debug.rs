use crate::graph::*;

// Should return true if graph exhibits the bug.
pub fn debug(g: &Graph) -> bool {
    let n = g.n.to_usize();
    // Test predomination.
    use crate::operator::domination::*;
    // h has vertices 1 and 2 of g swapped.
    let mut new_adj_list: Vec<Vec<usize>> = vec![vec![]; n];
    fn flip(x: usize) -> usize { if x == 0 { 1 } else if x == 1 { 0 } else { x } }
    for u in 0..n {
        for v in g.adj_list[u].iter() {
            new_adj_list[flip(u)].push(flip(*v));
        }
    }
    let h = Graph::of_adj_list(new_adj_list, crate::constructor::Constructor::Special);

    let g_one_dom = domination_number_with_predominations(g, 1);
    let g_two_dom = domination_number_with_predominations(g, 2);
    println!("H!");
    h.print();
    println!();
    let h_one_dom = domination_number_with_predominations(&h, 1);
    let h_two_dom = domination_number_with_predominations(&h, 2);
    println!("g_one: {}, g_two: {}", g_one_dom, g_two_dom);
    println!("h_one: {}, h_two: {}", h_one_dom, h_two_dom);
    println!("Are isomorphic: {}", g.is_isomorphic_to(&h));
    println!("comm: {}", h.is_adj_commutative());
    h.print();
    g_one_dom != h_two_dom || g_two_dom != h_one_dom
}