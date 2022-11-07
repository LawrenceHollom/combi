use std::fmt;
use std::time::*;

use utilities::*;

use crate::instruction::*;
use crate::operator::Operator;
use crate::graph::Graph;

pub struct Tabulation {
    pub order: Order,
    pub start: f64,
    pub end: f64,
    pub step: f64,
}

pub enum Controller {
    Single(Constructor),
    Repeat(Constructor, usize),
    Tabulate(Tabulation),
    Until(Constructor, BoolOperation),
    SearchTrees(usize, BoolOperation, Degree),
    KitchenSink(BoolOperation),
}

pub struct Instruction {
    pub controller: Controller,
    pub operations: Vec<Operation>,
}

impl Tabulation {
    pub fn new(order: Order, start: f64, end: f64, step: f64) -> Tabulation {
        Tabulation { order, start, end, step }
    }
}

impl fmt::Display for Tabulation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Tabulate results for simulations of G({}, p) for p from {} to {} in steps of {}",
            self.order, self.start, self.end, self.step)
    }
}

impl Controller {
    pub fn of_string(text: &str) -> Controller {
        let (func, args) = parse_function_like(text);

        match func.to_lowercase().as_str() {
            "rep" | "repeat" => {
                Controller::Repeat(Constructor::of_string(args[0]), 
                    args[1].parse().unwrap())
            },
            "tabulate" | "tab" => {
                Controller::Tabulate(Tabulation::new(Order::of_string(args[0]),
                    args[1].parse().unwrap(), args[2].parse().unwrap(), 
                    args[3].parse().unwrap()))
            },
            "until" => {
                Controller::Until(Constructor::of_string(args[0]), 
                    BoolOperation::of_string_result(args[1]).unwrap())
            },
            "search_trees" | "trees" => {
                let n: usize = args[0].parse().unwrap();
                Controller::SearchTrees(n, 
                    BoolOperation::of_string_result(args[1]).unwrap(),
                    if args.len() >= 3 { Degree::of_string(args[2]) } else { Degree::of_usize(n) })
            }
            "kitchen" | "kitchen_sink" | "sink" => {
                Controller::KitchenSink(BoolOperation::of_string_result(args[0]).unwrap())
            }
            &_ => {
                Controller::Single(Constructor::of_string(text))
            }
        }
    }
}

impl fmt::Display for Controller {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Controller::Repeat(constr, num) => {
                write!(f, "Repeat ({}) {} times", constr, num)
            },
            Controller::Tabulate(tabulation) => {
                write!(f, "{}", tabulation)
            },
            Controller::Until(constr, condition) => {
                write!(f, "Repeat ({}) until ({})", constr, condition)
            },
            Controller::Single(constr) => {
                write!(f, "{}", constr)
            }
            Controller::SearchTrees(n, condition, max_degree) => {
                write!(f, "Search all trees of order {} and max_degree {} until ({})", n, max_degree, condition)
            }
            Controller::KitchenSink(condition) => {
                write!(f, "Search the kitchen sink for a graph satisfying ({})", condition)
            }
        }
    }
}

impl Instruction {
    pub fn of_string(text: &str) -> Instruction {
        let re = regex::Regex::new(r"->|=>").unwrap();
        let pars: Vec<&str> = re.split(text).map(|par| par.trim()).collect();
        let controller = Controller::of_string(pars[0]);
        let operations = pars[1].split(',')
            .map(|str| Operation::of_string(str)).collect();

        Instruction {
            controller,
            operations,
        }
    }

    pub fn operations_string(&self) -> String {
        let operations_strs: Vec<String> = self.operations.iter().map(|x| format!("{x}")).collect();
        operations_strs.join(", ")
    }

    fn execute_single_return(&self, constr: &Constructor) -> Graph {
        let g = Graph::new(constr);
        let rep_start = SystemTime::now();
        
        let mut operator = Operator::new();
        let numbers: Vec<String> = self.operations
                .iter()
                .map(|op| operator.operate(&g, op))
                .collect();
        println!("{}: [{}], time: {}", self.operations_string(), numbers.join(", "),
                rep_start.elapsed().unwrap().as_millis());
        g
    }

    fn execute_single(&self, constr: &Constructor) {
        let _ = self.execute_single_return(constr);
    }

    fn execute_reps(&self, constr: &Constructor, reps: usize) {
        for rep in 0..reps {
            print!("{}: ", rep);
            self.execute_single(constr);
        }
    }

    fn execute_tabulate(&self, tab: &Tabulation) {
        let reps = 100;
        let propn = f64::round((tab.end - tab.start) / tab.step) as usize;
        let operators: Vec<FloatOperation> = self.operations
            .iter()
            .map(|x| match x {
                Operation::Int(op) => Some(FloatOperation::OfInt(*op)),
                Operation::Bool(op) => Some(FloatOperation::OfBool(*op)),
                Operation::Float(op) => Some(*op),
                Operation::Unit(_op) => None,
            })
            .flatten()
            .collect();
        let mut rows = vec![];
        for i in 0..=propn {
            let mut sums = vec![0.0; operators.len()];
            let p = tab.start + (tab.step * (i as f64));
            for _j in 0..reps {
                let constr = Constructor::ErdosRenyi(tab.order, p);
                let g = Graph::new(&constr);
                let mut operator = Operator::new();
                for (op, sum) in operators.iter().zip(sums.iter_mut()) {
                    *sum += operator.operate_float(&g, op);
                }
            }
            for sum in sums.iter_mut() {
                *sum /= reps as f64
            }
            print!("p: {:.4}", p);
            for x in sums.iter() {
                print!(" {:.4}", x);
            }
            println!();
            rows.push(sums);
        }
    }

    fn execute_until(&self, constr: &Constructor, condition: &BoolOperation) {
        let mut satisfied = false;
        let mut rep = 0;
        while !satisfied {
            print!("{}: ", rep);
            rep += 1;
            let g = self.execute_single_return(constr);
            let mut operator = Operator::new();
            if operator.operate_bool(&g, condition) {
                println!("Condition satisfied! {}", condition);
                println!("Graph: ");
                g.print();
                satisfied = true;
            }
        }
    }

    fn search_trees(&self, n: usize, condition: &BoolOperation, max_degree: &Degree) {
        fn construct_tree(n: usize, parents: &Vec<usize>, condition: &BoolOperation) -> bool {
            // Check all children of any node are in decreasing order of degree.
            let mut num_children: Vec<usize> = vec![0; n];
            for i in 0..(n-1) {
                num_children[parents[i]] += 1;
            }
            let mut is_in_order = true;
            for i in 0..(n-2) {
                if parents[i] == parents[i+1] && num_children[i+1] < num_children[i+2] {
                    is_in_order = false;
                }
            }
            if is_in_order {
                let mut operator = Operator::new();
                // We just copy the vector of parents because fuck it, that's why.
                let g = Graph::new(&Constructor::RootedTree(parents.to_vec()));
                
                let success = operator.operate_bool(&g, condition);
                if success {
                    println!("Success!");
                    g.print();
                }

                success
            } else {
                false
            }
        }

        fn search_trees_rec(n: usize, pos: usize, min: usize, parents: &mut Vec<usize>, condition: &BoolOperation, max_deg: usize) -> bool {
            if pos == 11 {
                println!("{:?}", parents);
                println!("{} {} {}", pos, min, max_deg);
            }

            if pos == n-1 {
                construct_tree(n, parents, condition)
            } else {
                for i in min..(pos+1) {
                    parents[pos] = i;
                    // change max deg so that 0 is the weakly highest degree node.
                    let new_max_deg = if min == 0 && i != 0 && pos < max_deg { pos } else { max_deg };
                    let min = if pos < max_deg - 1 { i } else { i.max(parents[(pos + 2) - max_deg] + 1)};
                    let success = search_trees_rec(n, pos + 1, min, parents, condition, new_max_deg);
                    if success {
                        return true;
                    }
                }
                false
            }
        }

        let mut parents = vec![0; n-1];
        let success = search_trees_rec(n, 0, 0, &mut parents, condition, max_degree.to_usize());
        if success {
            println!("Above tree found!")
        } else {
            println!("No tree found!")
        }
    }

    fn kitchen_sink(&self, _condition: &BoolOperation) {

    }

    pub fn execute(&self) {
        match &self.controller {
            Controller::Single(constr) => self.execute_single(constr),
            Controller::Repeat(constr, reps) => self.execute_reps(constr, *reps),
            Controller::Tabulate(tabulation) => self.execute_tabulate(tabulation),
            Controller::Until(constr, condition) => self.execute_until(constr, condition),
            Controller::SearchTrees(n, condition, max_degree) => self.search_trees(*n, condition, max_degree),
            Controller::KitchenSink(condition) => self.kitchen_sink(condition),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Constructor: {}\nOperations: [{}]", self.controller, self.operations_string())
    }
}