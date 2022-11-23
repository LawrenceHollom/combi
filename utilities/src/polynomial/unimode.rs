use crate::polynomial::*;

pub fn find_zero_in_range(f: &Polynomial, start: f64, end: f64) -> f64 {
    let mid = (end + start) / 2.0;
    if (end - start) < 0.0001 {
        mid
    } else if (f.evaluate(start) > 0.0) ^ (f.evaluate(mid) > 0.0) {
        find_zero_in_range(f, start, mid)
    } else {
        find_zero_in_range(f, mid, end)
    }
}

pub fn find_unimode(f: &Polynomial, start: f64, end: f64) -> Result<f64, &'static str> {
    let df = f.differentiate();
    if f.is_zero() {
        return Err("polynomial is identically zero");
    } else if df.is_zero() {
        return Err("polynomial is constant");
    }
    let mut is_positive = df.evaluate(start * 0.995 + end * 0.005) > 0.0;
    let mut num_changes = 0;
    let mut extremum_ub = 0.0;
    let mut extremum_lb = 0.0;

    fn get_point(start: f64, end: f64, i: i32) -> f64 {
        let propn = f64::from(i) / 100.0;
        propn * end + (1.0 - propn) * start
    }

    'search: for i in 1..100 {
        let x = get_point(start, end, i);
        let gradient = df.evaluate(x);
        if (gradient > 0.0) ^ is_positive {
            num_changes += 1;
            extremum_lb = get_point(start, end, i - 1);
            extremum_ub = x;
            is_positive = gradient > 0.0;
        }
        if num_changes >= 2 {
            break 'search;
        }
    }
    if num_changes == 0 {
        Err("no local extrema found")
    } else if num_changes >= 2 {
        Err("Multiple local extrema found")
    } else {
        Ok(find_zero_in_range(&df, extremum_lb, extremum_ub))
    }
}