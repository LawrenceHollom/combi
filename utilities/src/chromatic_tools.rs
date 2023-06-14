use crate::vertex_tools::*;
use crate::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Config(u128);

pub struct Coder {
    n: Order,
    k: usize,
    pows: VertexVec<u128>,
}

impl Coder {
    pub fn new(n: Order, k: usize) -> Coder {
        if (n.to_usize() as f64) * (k as f64 + 1.0).log2() >= 128.0 {
            panic!("Parameters too large for chromatic_tools! (n, k) = ({}, {})", n, k);
        }
        let mut pows = VertexVec::new(n, &0);
        for (i, v) in n.iter_verts().enumerate() {
            pows[v] = ((k + 1) as u128).pow(i as u32);
        }
        Coder { n, k, pows }
    }
    
    pub fn get_colour(&self, config: Config, u: &Vertex) -> Option<usize> {
        let col = ((config.0 / self.pows[*u]) % ((self.k + 1) as u128)) as usize;
        if col == self.k {
            None
        } else {
            Some(col)
        }
    }

    pub fn play_move(&self, config: Config, v: Vertex, col: usize) -> Config {
        Config(config.0 - ((self.k - col) as u128 * self.pows[v]))
    }

    pub fn increase_k(&self, other: &Coder, config: Config) -> Config {
        let mut out = other.get_start_config();
        for v in self.n.iter_verts() {
            if let Some(col) = self.get_colour(config, &v) {
                out = other.play_move(out, v, col);
            }
        }
        out
    }

    pub fn vertex_set(&self, config: Config) -> VertexSet {
        let mut out = VertexSet::new(self.n);
        for v in self.n.iter_verts() {
            match self.get_colour(config, &v) {
                Some(_) => out.add_vert(v),
                None => (),
            }
        }
        out
    }

    pub fn get_start_config(&self) -> Config {
        let mut config = 0;
        for pow in self.pows.iter() {
            config += *pow * self.k as u128;
        }
        Config(config)
    }

    // Returns an index with every wlog assumption made that we can make.
    pub fn get_wlog_index(&self, config: Config, fix_last_col: bool) -> Config {
        let mut map = vec![None; self.k + 1];
        map[self.k] = Some(self.k);
        if fix_last_col {
            map[self.k - 1] = Some(self.k - 1);
        }
        let mut wlog_index = 0;
        let mut next_col = 0;

        for pow in self.pows.iter() {
            let digit = ((config.0 / pow) % (self.k + 1) as u128) as usize;
            if map[digit].is_none() {
                map[digit] = Some(next_col);
                next_col += 1;
            }
            wlog_index += map[digit].unwrap() as u128 * pow;
        }
        Config(wlog_index)
    }

    pub fn print(&self, config: Config) {
        for v in self.n.iter_verts() {
            print!("{} ", self.get_colour(config, &v).map_or("-".to_owned(), |col| col.to_string()));
        }
        println!();
    }
}