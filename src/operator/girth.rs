use crate::graph::*;

pub fn girth(g: &Graph) -> u32 {
    let dist = g.floyd_warshall();
    let mut girth = g.n.to_usize();

    for (u, d) in dist.iter_enum() {
        for v in u.incr().iter_from(g.n) {
            // If they're far away there's no point trying
            if d[v] < (girth + 1) / 2 {
                let mut num_less = 0;
                let mut num_equal = 0;
                for w in g.adj_list[v].iter() {
                    use std::cmp::Ordering::*;
                    match d[*w].cmp(&d[v]) {
                        Less => num_less += 1,
                        Equal => num_equal += 1,
                        Greater => ()
                    }
                }
                if num_less >= 2 {
                    girth = 2 * d[v];
                } else if num_equal >= 1 {
                    girth = 2 * d[v] + 1;
                }
            }
        }
    }

    girth as u32
}