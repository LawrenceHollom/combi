use std::fmt;

mod unimode;

pub struct Polynomial {
    coefs: Vec<i64>,
    var_name: String,
}

impl Polynomial {
    pub fn new() -> Polynomial {
        Polynomial { coefs: vec![], var_name: "p".to_owned() }
    }

    pub fn of_vec(coefs: &Vec<i64>) -> Polynomial {
        let mut new_coefs: Vec<i64> = vec![];
        for x in coefs {
            new_coefs.push(*x);
        }
        Polynomial { coefs: new_coefs, var_name: "p".to_owned() }
    }

    pub fn monomial(coef: i64, power: usize) -> Polynomial {
        let mut coefs = vec![0; power];
        coefs.push(coef);
        Polynomial { coefs, var_name: "p".to_owned() }
    }

    fn copy(&self) -> Polynomial {
        let mut coefs: Vec<i64> = vec![];
        for x in self.coefs.iter() {
            coefs.push(*x);
        }
        Polynomial { coefs, var_name: self.var_name.to_owned() }
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

    fn add_vec_inplace(coefs: &mut Vec<i64>, rhs: &Polynomial) {
        for (pos, y) in rhs.coefs.iter().enumerate() {
            if pos >= coefs.len() {
                coefs.push(*y);
            } else {
                coefs[pos] += *y;
            }
        }
    }

    pub fn add(&self, rhs: &Polynomial) -> Polynomial {
        let mut coefs: Vec<i64> = vec![];
        for x in self.coefs.iter() {
            coefs.push(*x);
        }
        Self::add_vec_inplace(&mut coefs, rhs);
        Polynomial::of_vec(&coefs)
    }

    pub fn add_inplace(&mut self, rhs: &Polynomial) {
        Self::add_vec_inplace(&mut self.coefs, rhs)
    }

    fn sub_vec_inplace(coefs: &mut Vec<i64>, rhs: &Polynomial) {
        for (pos, y) in rhs.coefs.iter().enumerate() {
            if pos >= coefs.len() {
                coefs.push(-*y);
            } else {
                coefs[pos] -= *y;
            }
        }
    }

    pub fn sub(&self, rhs: &Polynomial) -> Polynomial {
        let mut coefs: Vec<i64> = vec![];
        for x in self.coefs.iter() {
            coefs.push(*x);
        }
        Self::sub_vec_inplace(&mut coefs, rhs);
        Polynomial::of_vec(&coefs)
    }

    pub fn sub_inplace(&mut self, rhs: &Polynomial) {
        Self::sub_vec_inplace(&mut self.coefs, rhs)
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

    pub fn apply(&self, g: &Polynomial) -> Polynomial {
        let mut out = Polynomial::new();
        for (i, xi) in self.coefs.iter().enumerate() {
            // Highly efficient, as always
            out.add_inplace(&g.pow(i).mul(&Self::of_vec(&vec![*xi])));
        }
        out
    }

    pub fn evaluate(&self, x: f64) -> f64 {
        let mut out = 0.0;
        for (i, xi) in self.coefs.iter().enumerate() {
            out += x.powf(i as f64) * (*xi as f64);
        }
        out
    }

    pub fn differentiate(&self) -> Polynomial {
        let mut coefs: Vec<i64> = vec![];
        for (i, xi) in self.coefs.iter().enumerate() {
            if i > 0 {
                coefs.push(*xi * (i as i64));
            }
        }
        Polynomial::of_vec(&coefs)
    }

    pub fn is_zero(&self) -> bool {
        self.coefs.iter().all(|x| *x == 0)
    }

    pub fn find_prob_unimode(&self) -> Result<f64, &'static str> {
        unimode::find_unimode(&self, 0.0, 1.0)
    }

    pub fn with_var_name(&self, var_name: &str) -> Polynomial {
        Polynomial { coefs: self.coefs.to_owned(), var_name: var_name.to_owned() }
    }
}

impl fmt::Display for Polynomial {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pars: Vec<String> = self.coefs
            .iter()
            .enumerate()
            .map(|(exp, c)| format!("{}{}^{}", c, self.var_name, exp)).collect();
        write!(f, "{}", pars.join(" + "))
    }
}