use crate::graph::*;

pub fn girth(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let mut dist = vec![vec![n; n]; n];
    for (i, d) in dist.iter_mut().enumerate() {
        d[i] = 0;
    }
    for (i, d) in dist.iter_mut().enumerate() {
        for j in g.adj_list[i].iter() {
            d[*j] = 1;
        }
    }
    for k in 0..n {
        for i in 0..n {
            for j in 0..n {
                if dist[i][j] > dist[i][k] + dist[k][j] {
                    dist[i][j] = dist[i][k] + dist[k][j];
                }
            }
        }
    }

    let mut girth = n;

    for (u, d) in dist.iter().enumerate().take(n-1) {
        for v in (u+1)..n {
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