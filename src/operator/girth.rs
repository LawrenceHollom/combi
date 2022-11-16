use crate::graph::*;

pub fn girth(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let mut dist = vec![vec![n; n]; n];
    for i in 0..n {
        dist[i][i] = 0;
    }
    for i in 0..n {
        for j in g.adj_list[i].iter() {
            dist[i][*j] = 1;
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

    for u in 0..(n-1) {
        for v in (u+1)..n {
            // If they're far away there's no point trying
            if dist[u][v] < (girth + 1) / 2 {
                let mut num_less = 0;
                let mut num_equal = 0;
                for w in g.adj_list[v].iter() {
                    if dist[u][*w] < dist[u][v] {
                        num_less += 1;
                    } else if dist[u][*w] == dist[u][v] {
                        num_equal += 1;
                    }
                }
                if num_less >= 2 {
                    girth = 2 * dist[u][v];
                } else if num_equal >= 1 {
                    girth = 2 * dist[u][v] + 1;
                }
            }
        }
    }

    girth as u32
}