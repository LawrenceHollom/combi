use std::fmt;

pub struct Polynomial {
    coefs: Vec<i64>,
}

impl Polynomial {
    pub fn new() -> Polynomial {
        Polynomial { coefs: vec![] }
    }

    pub fn of_vec(coefs: &Vec<i64>) -> Polynomial {
        let mut new_coefs: Vec<i64> = vec![];
        for x in coefs {
            new_coefs.push(*x);
        }
        Polynomial { coefs: new_coefs }
    }

    fn copy(&self) -> Polynomial {
        let mut coefs: Vec<i64> = vec![];
        for x in self.coefs.iter() {
            coefs.push(*x);
        }
        Polynomial { coefs }
    }

    pub fn pow(&self, exp: usize) -> Polynomial {
        if exp == 0 {
            Polynomial::of_vec(&vec![1])
        } else if exp == 1 {
            self.copy()
        } else {
            self.copy().mul(&self.pow(exp - 1))
        }
    }

    pub fn add(&self, rhs: &Polynomial) -> Polynomial {
        let mut coefs: Vec<i64> = vec![];
        for x in self.coefs.iter() {
            coefs.push(*x);
        }
        for (pos, y) in rhs.coefs.iter().enumerate() {
            if pos >= coefs.len() {
                coefs.push(*y);
            } else {
                coefs[pos] += *y;
            }
        }
        Polynomial::of_vec(&coefs)
    }

    pub fn mul(&self, rhs: &Polynomial) -> Polynomial {
        let mut coefs: Vec<i64> = vec![];
        for (i, xi) in self.coefs.iter().enumerate() {
            for (j, yj) in rhs.coefs.iter().enumerate() {
                if i + j >= coefs.len() {
                    coefs.push(xi * yj);
                } else {
                    coefs[i + j] += xi * yj;
                }
            }
        }
        Polynomial::of_vec(&coefs)
    }
}

impl fmt::Display for Polynomial {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pars: Vec<String> = self.coefs
            .iter()
            .enumerate()
            .map(|(exp, c)| format!("{}p^{}", c, exp)).collect();
        write!(f, "{}", pars.join(" + "))
    }
}