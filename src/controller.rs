use std::collections::HashSet;
use std::fmt;
use std::time::*;
use std::io::*;

use rand::Rng;
use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;
use rand::thread_rng;
use utilities::*;
use utilities::edge_tools::*;
use utilities::vertex_tools::*;

use crate::constructor::*;
use crate::operation::int_operation::IntOperation;
use crate::operation::string_list_operation::StringListOperation;
use crate::dossier::AnnotationsBox;
use crate::dossier::Operator;
use crate::entity::graph::Graph;
use crate::entity::Entity;

use crate::operation::*;
use crate::operation::bool_operation::*;
use crate::operation::rational_operation::*;

pub struct Tabulation {
    pub order: Order,
    pub start: f64,
    pub end: f64,
    pub step: f64,
}

pub enum Instruction {
    Single(Constructor),
    Repeat(Constructor, usize),
    Tabulate(Tabulation),
    Until(Constructor, Vec<BoolOperation>),
    Forever(Constructor, Vec<BoolOperation>),
    SearchTrees(usize, BoolOperation, Degree),
    KitchenSink(Vec<BoolOperation>),
    KitchenSinkAll(Vec<BoolOperation>),
    Process(Order),
    BipartiteProcess(Order),
    ProcessUntil(Order, Vec<BoolOperation>),
    ProcessUntilDecreasing(Order, IntOperation, Vec<BoolOperation>),
    SearchAll(Order, Vec<BoolOperation>),
    Collate(Constructor, usize, StringListOperation),
    Help,
}

pub struct Controller {
    pub instruction: Instruction,
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

impl Instruction {
    pub fn of_string(text: &str) -> Instruction {
        use Instruction::*;
        let (func, args) = parse_function_like(text);

        match func.to_lowercase().as_str() {
            "rep" | "repeat" => {
                Repeat(Constructor::of_string(args[0]), 
                    args[1].parse().unwrap())
            },
            "tabulate" | "tab" => {
                Tabulate(Tabulation::new(Order::of_string(args[0]),
                    args[1].parse().unwrap(), args[2].parse().unwrap(), 
                    args[3].parse().unwrap()))
            },
            "until" => {
                Until(Constructor::of_string(args[0]), 
                    args.iter().skip(1).map(|arg| BoolOperation::of_string_result(arg).unwrap()).collect())
            },
            "forever" => {
                Forever(Constructor::of_string(args[0]), 
                    args.iter().skip(1).map(|arg| BoolOperation::of_string_result(arg).unwrap()).collect())
            },
            "search_trees" | "trees" => {
                let n: usize = args[0].parse().unwrap();
                SearchTrees(n, BoolOperation::of_string_result(args[1]).unwrap(),
                    if args.len() >= 3 { Degree::of_string(args[2]) } else { Degree::of_usize(n) })
            }
            "kitchen" | "kitchen_sink" | "sink" => {
                KitchenSink(args.iter().map(|arg| 
                    BoolOperation::of_string_result(*arg).unwrap()).collect())
            }
            "sink_all" => {
                KitchenSinkAll(args.iter().map(|arg| 
                    BoolOperation::of_string_result(*arg).unwrap()).collect())
            }
            "process" | "proc" => {
                Process(Order::of_string(args[0]))
            }
            "bip_process" | "bip_proc" => {
                BipartiteProcess(Order::of_string(args[0]))
            }
            "process_until" | "proc_until" => {
                ProcessUntil(Order::of_string(args[0]),
                    args.iter().skip(1).map(|arg| BoolOperation::of_string_result(*arg).unwrap()).collect())
            }
            "process_until_decreasing" | "proc_until_dec" => {
                ProcessUntilDecreasing(Order::of_string(args[0]), IntOperation::of_string_result(args[1]).unwrap(),
                    args.iter().skip(2).map(|arg| BoolOperation::of_string_result(*arg).unwrap()).collect())
            }
            "search_all" | "all" => {
                SearchAll(Order::of_string(args[0]),
                    args.iter().skip(1).map(|arg| BoolOperation::of_string_result(*arg).unwrap()).collect())
            }
            "collate" => {
                Collate(Constructor::of_string(args[0]), args[1].parse().unwrap(),
                    StringListOperation::of_string_result(args[2]).unwrap())
            }
            "help" | "h" => Help,
            &_ => {
                Single(Constructor::of_string(text))
            }
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            Repeat(constr, num) => {
                write!(f, "Repeat ({}) {} times", constr, num)
            },
            Tabulate(tabulation) => {
                write!(f, "{}", tabulation)
            },
            Until(constr, conditions) => {
                write!(f, "Repeat ({}) until ({:?})", constr, conditions)
            },
            Forever(constr, conditions) => {
                write!(f, "Repeat ({}) forever, processing whenever ({:?})", constr, conditions)
            },
            Single(constr) => {
                write!(f, "{}", constr)
            }
            SearchTrees(n, condition, max_degree) => {
                write!(f, "Search all trees of order {} and max_degree {} until ({})", n, max_degree, condition)
            }
            KitchenSink(conditions) => {
                write!(f, "Search the kitchen sink for a graph satisfying {:?}", *conditions)
            }
            KitchenSinkAll(conditions) => {
                write!(f, "Search the kitchen sink for all graphs satisfying {:?}", *conditions)
            }
            Process(order) => {
                write!(f, "Random graph process of order {}", order)
            }
            BipartiteProcess(order) => {
                write!(f, "Random bipartite graph process of order {}", order)
            }
            ProcessUntil(order, conditions) => {
                write!(f, "Run graph processes of order {} until {:?}", order, conditions)
            }
            ProcessUntilDecreasing(order, int_op, conditions) => {
                write!(f, "Run graph processes of order {} until {:?} decreases from one graph to the next and {:?} holds",
                    order, int_op, conditions)
            }
            SearchAll(order, conditions) => {
                write!(f, "Search all graphs of order {} until {:?}", order, conditions)
            }
            Collate(constr, reps, op) => {
                write!(f, "Collate {} values of ({}) over {} reps", op, constr, reps)
            }
            Help => write!(f, "Print help text"),
        }
    }
}

impl Controller {
    pub fn of_string(text: &str) -> Controller {
        /*let re = regex::Regex::new(r"->|=>").unwrap();
        let pars: Vec<&str> = re.split(text).map(|par| par.trim()).collect();*/
        let pars = match parse_infix_like_restricted(text, vec!['-', '=', '>']) {
            Some((par1, _op, par2)) => vec![par1, par2],
            None => vec![text.trim()],
        };

        let controller = Instruction::of_string(pars[0]);
        let operations = 
            if pars.len() > 1 {
                split_list(pars[1]).iter().map(|x| Operation::of_string(x)).collect()
            } else {
                vec![]
            };

        Controller {
            instruction: controller,
            operations,
        }
    }

    pub fn operations_string(&self) -> String {
        let operations_strs: Vec<String> = self.operations.iter().map(|x| format!("{x}")).collect();
        operations_strs.join(", ")
    }

    fn compute_and_print(&self, operator: &mut Operator, ann: &mut AnnotationsBox) {
        let rep_start = SystemTime::now();
        let numbers: Vec<String> = self.operations
                .iter()
                .map(|op| operator.operate(ann, op))
                .collect();
        println!("{}: [{}], time: {}", self.operations_string(), numbers.join(", "),
            rep_start.elapsed().unwrap().as_millis());
    }

    fn execute_single_return(&self, constr: &Constructor) -> Operator {
        let g = constr.new_entity();
        let mut operator = Operator::new(g);
        let mut ann = AnnotationsBox::new();
        self.compute_and_print(&mut operator, &mut ann);
        operator
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
        use Operation::*;
        let operators: Vec<RationalOperation> = self.operations
            .iter()
            .filter_map(|x| match x {
                Int(op) => Some(RationalOperation::OfInt(*op)),
                Bool(op) => Some(RationalOperation::OfBool(Box::new(op.to_owned()))),
                Rational(op) => Some(op.to_owned()),
                Unit(_op) => None,
                StringList(_op) => None,
            })
            .collect();
        let mut rows = vec![];
        for i in 0..=propn {
            let mut sums = vec![0.0; operators.len()];
            let p = tab.start + (tab.step * (i as f64));
            for _j in 0..reps {
                let constr = Constructor::Random(RandomConstructor::ErdosRenyi(tab.order, p));
                let g = constr.new_entity();
                let mut operator = Operator::new(g);
                let mut ann = AnnotationsBox::new();
                for (op, sum) in operators.iter().zip(sums.iter_mut()) {
                    *sum += operator.operate_rational(&mut ann, op).to_f64();
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

    fn get_shuffled_edges(&self, order: Order, rng: &mut ThreadRng) -> Vec<Edge> {
        let mut edges = vec![];
        for (u, v) in order.iter_pairs() {
            edges.push(Edge::of_pair(u, v));
        }
        edges.shuffle(rng);
        edges
    }

    fn execute_process(&self, order: Order) {
        // Add the edges one by one, and run on each graph from empty to complete.
        let mut rng = thread_rng();
        let edges = self.get_shuffled_edges(order, &mut rng);
        println!("Edges: {:?}", edges);
        let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);

        for (i, e) in edges.iter().enumerate() {
            adj_list[e.fst()].push(e.snd());
            adj_list[e.snd()].push(e.fst());
            let g = Graph::of_adj_list(adj_list.to_owned(), Constructor::Special);
            let mut ann = AnnotationsBox::new();
            let mut operator = Operator::new(Entity::Graph(g));
            print!("[{} / {}]: ", i+1, edges.len());
            self.compute_and_print(&mut operator, &mut ann);
        }
    }

    fn execute_bipartite_process(&self, order: Order) {
        // Add the edges one by one, and run on each graph from empty to complete.
        let mut rng = thread_rng();
        let mut part = VertexVec::new(order, &false);
        let mut part_size = 0;
        let mut copart_size = order.to_usize();
        for v in order.iter_verts() {
            if rng.gen_bool(0.5) {
                part[v] = true;
                part_size += 1;
                copart_size -= 1;
            }
        }
        let edges = self.get_shuffled_edges(order, &mut rng);
        println!("Edges: {:?}", edges);
        let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);
        let mut count = 0;

        for e in edges.iter() {
            if part[e.fst()] != part[e.snd()] {
                adj_list[e.fst()].push(e.snd());
                adj_list[e.snd()].push(e.fst());
                let g = Graph::of_adj_list(adj_list.to_owned(), Constructor::Special);
                let mut ann = AnnotationsBox::new();
                let mut operator = Operator::new(Entity::Graph(g));
                count += 1;
                print!("[{} / {}]: ", count, part_size * copart_size);
                self.compute_and_print(&mut operator, &mut ann);
            }
        }
    }

    fn do_all_conditions_hold(&self, operator: &mut Operator, ann_box: &mut AnnotationsBox, 
            conditions: &[BoolOperation], should_print: bool) -> bool {
        let mut is_good = true;
        'test_conditions: for operation in conditions.iter() {
            if !operator.operate_bool(ann_box, operation) {
                is_good = false;
                break 'test_conditions;
            }
        }
        if is_good && should_print {
            println!("Found graph satisfying conditions!");
            operator.print_all();
            operator.print_entity();
        }
        is_good
    }

    fn execute_process_until(&self, order: Order, conditions: &[BoolOperation]) {
        let mut rng = thread_rng();
        let mut rep = 0;
        let start_time = SystemTime::now();
        'test_procs: loop {
            let edges = self.get_shuffled_edges(order, &mut rng);
            let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);

            for e in edges.iter() {
                adj_list[e.fst()].push(e.snd());
                adj_list[e.snd()].push(e.fst());
                let g = Graph::of_adj_list(adj_list.to_owned(), Constructor::Special);
                let mut operator = Operator::new(Entity::Graph(g));
                let mut ann_box = AnnotationsBox::new();
                if self.do_all_conditions_hold(&mut operator, &mut ann_box, conditions, true) {
                    break 'test_procs;
                }
            }
            rep += 1;
            if rep <= 10 || (rep <= 100 && rep % 10 == 0) || (rep <= 1000 && rep % 100 == 0) || rep % 1000 == 0 {
                let avg_time = start_time.elapsed().unwrap().as_millis() / rep;
                println!("Completed process {}, no example found. Average duration: {}", rep, avg_time);
            }
        }
    }

    fn execute_process_until_decreasing(&self, order: Order, int_op: &IntOperation, conditions: &[BoolOperation]) {
        let mut rng = thread_rng();
        let mut rep = 0;
        let start_time = SystemTime::now();
        'test_procs: loop {
            let edges = self.get_shuffled_edges(order, &mut rng);
            let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);
            let mut last_value_opn = None;

            for e in edges.iter() {
                adj_list[e.fst()].push(e.snd());
                adj_list[e.snd()].push(e.fst());
                let g = Graph::of_adj_list(adj_list.to_owned(), Constructor::Special);
                let mut operator = Operator::new(Entity::Graph(g));
                let mut ann_box = AnnotationsBox::new();
                if self.do_all_conditions_hold(&mut operator, &mut ann_box, conditions, false) {
                    let new_value = operator.operate_int(&mut ann_box, int_op);
                    if let Some(last_value) = last_value_opn {
                        if last_value > new_value {
                            println!("Decreasement found! g:");
                            operator.print_entity();
                            println!("After adding edge {}", e);
                            break 'test_procs;
                        }
                    }
                    last_value_opn = Some(new_value)
                } else {
                    last_value_opn = None;
                }
            }
            rep += 1;
            if rep <= 10 || (rep <= 100 && rep % 10 == 0) || (rep <= 1000 && rep % 100 == 0) || rep % 1000 == 0 {
                let avg_time = start_time.elapsed().unwrap().as_millis() / rep;
                println!("Completed process {}, no example found. Average duration: {}", rep, avg_time);
            }
        }
    }

    fn execute_search_all(&self, order: Order, conditions: &[BoolOperation]) {
        let mut adj = VertexVec::new(order, &VertexVec::new(order, &false));
        let mut degs = VertexVec::new(order, &Degree::ZERO);
        let indexer = EdgeIndexer::new_complete(order);

        let num_edges = (order.to_usize() * (order.to_usize() - 1)) / 2;
        let mut found_graph = false;

        'test_all_graphs: for code in 1..(1_u128 << num_edges) {
            let mut i = 0;
            let mut sta = code;
            while sta % 2 == 0 {
                let e = indexer.invert(i);
                adj[e.fst()][e.snd()] = false;
                adj[e.snd()][e.fst()] = false;
                degs[e.fst()].decr_inplace();
                degs[e.snd()].decr_inplace();
                i += 1;
                sta /= 2;
            }
            let e = indexer.invert(i);
            adj[e.fst()][e.snd()] = true;
            adj[e.snd()][e.fst()] = true;
            degs[e.fst()].incr_inplace();
            degs[e.snd()].incr_inplace();

            // If this graph has decreasing degree sequence, then actually run it.
            let mut is_good = true;
            'test_degs: for (i, deg) in degs.iter().enumerate().skip(1) {
                if *deg > degs[Vertex::of_usize(i - 1)] {
                    is_good = false;
                    break 'test_degs;
                }
            }

            if is_good {
                let g = Graph::of_matrix(adj.to_owned(), Constructor::Special);
                let mut operator = Operator::new(Entity::Graph(g));
                let mut ann_box = AnnotationsBox::new();
                if self.do_all_conditions_hold(&mut operator, &mut ann_box, conditions, true) {
                    found_graph = true;
                    break 'test_all_graphs;
                }
            }
        }
        if !found_graph {
            println!("No graph found.");
        }
    }

    fn execute_collate(&self, constr: &Constructor, op: &StringListOperation, reps: usize) {
        let mut vals: HashSet<String> = HashSet::new();
        let mut last_index_of_change = 0;
        for i in 0..reps {
            let mut operator = Operator::new(constr.new_entity());
            let mut ann_box = AnnotationsBox::new();
            for line in operator.operate_string_list(&mut ann_box, op) {
                if vals.insert(line) {
                    // i.e. if the line wasn't there before
                    last_index_of_change = i;
                }
            }
        }
        let mut i = 0;
        let mut lines: Vec<&String> = vals.iter().collect();
        lines.sort();
        for line in lines.iter() {
            i += 1;
            println!("{}", line)
        }
        println!("{} distinct values found! Last index of change: {}", i, last_index_of_change);
    }

    fn execute_until(&self, constr: &Constructor, conditions: &[BoolOperation], forever: bool) {
        let mut satisfied = false;
        let mut rep = 0;
        let mut lines_printed = 0;
        let mut round_lines_printed = 0;
        let mut checkpoint_time = SystemTime::now();
        let mut required_steps = 0;
        let mut regular_rep_printing = 1000;
        while !satisfied || forever {
            let mut checkpointed = false;
            if lines_printed >= 30 {
                if checkpoint_time.elapsed().unwrap().as_secs() < 10 {
                    required_steps += 1;
                    println!("Cutting verbosity!");
                }
                checkpointed = true;
            } else if round_lines_printed >= 5 {
                if checkpoint_time.elapsed().unwrap().as_secs() < 10 {
                    regular_rep_printing *= 10;
                    println!("Cutting round-number verbosity! {} {}", rep, regular_rep_printing );
                }
                checkpointed = true;
            }
            if checkpointed {
                lines_printed = 0;
                round_lines_printed = 0;
                checkpoint_time = SystemTime::now();
            }
            let mut printed_number = false;
            if required_steps == 0 || rep % regular_rep_printing == 0 {
                pretty_print_int(rep, ": ");
                printed_number = true;
                if rep % regular_rep_printing == 0 {
                    round_lines_printed += 1;
                } else {
                    lines_printed += 1;
                }
            }
            rep += 1;
            let g = constr.new_entity();
            let mut ann = AnnotationsBox::new();
            let mut operator = Operator::new(g);
            let mut all_satisfied = true;
            let mut num_satisfied = 0;
            'test_conditions: for condition in conditions.iter() {
                if operator.operate_bool(&mut ann, condition) {
                    num_satisfied += 1;
                    if num_satisfied >= required_steps {
                        if !printed_number {
                            pretty_print_int(rep, ": ");
                            for _i in 0..(required_steps - 1) {
                                print!("X ");
                            }
                            printed_number = true;
                            lines_printed += 1;
                        }
                        print!("X ");
                        std::io::stdout().flush().unwrap();
                    }
                    
                } else {
                    all_satisfied = false;
                    break 'test_conditions;
                }
            }
            if printed_number {
                operator.print_all();
            }
            if all_satisfied {
                if !forever {
                    println!("All conditions satisfied! {:?}", conditions);
                    println!("Entity: ");
                    operator.print_entity();
                }
                self.compute_and_print(&mut operator, &mut ann);
                satisfied = true;
            }
        }
    }

    fn search_trees(&self, n: usize, condition: &BoolOperation, max_degree: &Degree) {
        fn construct_tree(n: usize, parents: &[usize], condition: &BoolOperation) -> bool {
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
                // We just copy the vector of parents because fuck it, that's why.
                let g = Constructor::RootedTree(parents.to_vec()).new_entity();
                let mut operator = Operator::new(g);
                let mut ann = AnnotationsBox::new();
                
                let success = operator.operate_bool(&mut ann, condition);
                if success {
                    println!("Success!");
                    operator.print_entity();
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
            
    fn test_sink_graph(&self, g: Graph, conditions: &[BoolOperation], find_all: bool) -> bool {
        let mut operator = Operator::new(Entity::Graph(g));
        let mut ann = AnnotationsBox::new();
        let mut result = true;
        'test_conditions: for condition in conditions.iter() {
            if !operator.operate_bool(&mut ann, condition) {
                result = false;
                break 'test_conditions;
            }
        }
        if result {
            if find_all {
                println!("{}", operator.get_constructor());
            } else {
                // print lots of information in this case.
                println!("Graph satisfying condition found!");
                println!("Constructor: {}", operator.get_constructor());
                operator.print_all();
                println!("Graph:");
                operator.print_entity();
            }
        }
        result
    }

    fn execute_kitchen_sink(&self, conditions: &[BoolOperation], find_all: bool) {
        use Constructor::*;
        use RawConstructor::*;

        let max_verts = 100;
        let mut constructors: Vec<Vec<Constructor>> = vec![vec![]; max_verts];
        let max_new_graphs = 100000;
        fn ous(n: usize) -> Order {
            Order::of_usize(n)
        }
        constructors[2].append(&mut vec![Raw(Complete(ous(2))), Raw(Empty(ous(2)))]);
        constructors[3].append(&mut vec![Raw(Complete(ous(3))), Raw(Path(ous(3))), Raw(Empty(ous(3)))]);

        for verts in 4..max_verts {
            let order = ous(verts);
            constructors[verts].push(Raw(Complete(order)));
            constructors[verts].push(Raw(Cyclic(order)));
            constructors[verts].push(Raw(Path(order)));
            constructors[verts].push(Raw(Star(order)));
            constructors[verts].push(Raw(Empty(order)));
            if verts % 4 == 2 && verts >= 10 {
                constructors[verts].push(Raw(Petersen(verts / 2, 2)));
                if verts >= 14 {
                    constructors[verts].push(Raw(Petersen(verts / 2, 3)));
                }
            }
        }
        constructors[6].push(Raw(Octahedron));
        constructors[12].push(Raw(Icosahedron));
        constructors[20].push(Raw(Dodecahedron));

        let mut verts = 4;
        let mut num_new_graphs = 0;
        
        'main: loop {
            let mut new_constructors: Vec<Constructor> = vec![];
            let mut graphs: Vec<Graph> = vec![];
            for constr in constructors[verts].iter() {
                // Actually test the graph
                let g = constr.new_entity().as_owned_graph();
                if self.test_sink_graph(g.clone(), conditions, find_all) && !find_all {
                    break 'main;
                }
                graphs.push(g);
            }
            'decompose: for factor in 2..verts {
                if factor * factor > verts {
                    break 'decompose;
                }
                if verts % factor == 0 {
                    // Add lots of new graphs
                    for (i, c1) in constructors[factor].iter().enumerate() {
                        for (j, c2) in constructors[verts / factor].iter().enumerate() {
                            // Don't construct duplicate products.
                            if factor < verts / factor || i <= j {
                                for product in ProductConstructor::all() {
                                    let constr = Product(product, Box::new((*c1).to_owned()), Box::new((*c2).to_owned()));
                                    let g = constr.new_entity().as_owned_graph();
                                    let mut is_already_there = false;
                                    'find_graph: for h in graphs.iter() {
                                        if h.is_isomorphic_to(&g) {
                                            is_already_there = true;
                                            break 'find_graph;
                                        }
                                    }
                                    if !is_already_there {
                                        // Actually test the graph
                                        if self.test_sink_graph(g.clone(), conditions, find_all) && !find_all {
                                            break 'main;
                                        }
                                        graphs.push(g);
                                        new_constructors.push(constr);
                                        num_new_graphs += 1;
                                        if num_new_graphs > max_new_graphs {
                                            println!("Hit cap! Breaking!");
                                            break 'decompose;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if verts >= constructors.len() {
                constructors.push(vec![]);
            }
            for c in new_constructors.iter() {
                constructors[verts].push((*c).to_owned());
            }
            
            println!("  Added kitchen sink graphs with {} vertices. There are {} of them.", verts, constructors[verts].len());
            if num_new_graphs >= max_new_graphs {
                break 'main;
            }

            if !find_all {
                // Test some random graphs for good measure
                let mut random_constructors: Vec<Constructor> = vec![];

                use RandomConstructor::*;
                let nf = verts as f64;
                random_constructors.push(Random(ErdosRenyi(ous(verts), 1.0 / nf)));
                for i in (-1)..4 {
                    random_constructors.push(Random(ErdosRenyi(ous(verts), (nf.ln() + (i as f64 * nf.ln().ln())) / nf)));
                }
                random_constructors.push(Random(ErdosRenyi(ous(verts), 0.1)));
                random_constructors.push(Random(ErdosRenyi(ous(verts), 0.2)));
                random_constructors.push(Random(ErdosRenyi(ous(verts), 0.5)));

                for _rep in 0..10 {
                    for constr in random_constructors.iter() {
                        let g = constr.new_entity().as_owned_graph();
                        if self.test_sink_graph(g, conditions, find_all) && !find_all {
                            break 'main;
                        }
                    }
                }
            }
            println!("  Completed graphs with {} vertices", verts);

            verts += 1;
        }
    }

    fn print_help() {
        println!("Options:");
        println!();
        println!("  [constructor]->[operations]");
        println!("     Run the given operations on that graph.");
        println!();
        println!("  rep([constructor],[number])->[operations]");
        println!("     Repeat running operation on (random) constructor [number] times.");
        println!();
        println!("  tab(order, start, stop, step)->[operations]");
        println!();
        println!("  until([constructor],[bool operation])->[operations]");
        println!();
        println!("  trees(order, [bool operation], optional max degree)->[operations]");
        println!();
        println!("  sink([bool operation])->[operations]");
        println!();
        println!("  help");
        println!();
    }

    pub fn execute(&self) {
        use Instruction::*;
        match &self.instruction {
            Single(constr) => self.execute_single(constr),
            Repeat(constr, reps) => self.execute_reps(constr, *reps),
            Tabulate(tabulation) => self.execute_tabulate(tabulation),
            Until(constr, conditions) => self.execute_until(constr, conditions, false),
            Forever(constr, conditions) => self.execute_until(constr, conditions, true),
            SearchTrees(n, condition, max_degree) => self.search_trees(*n, condition, max_degree),
            KitchenSink(conditions) => self.execute_kitchen_sink(conditions, false),
            KitchenSinkAll(conditions) => self.execute_kitchen_sink(conditions, true),
            Process(order) => self.execute_process(*order),
            BipartiteProcess(order) => self.execute_bipartite_process(*order),
            ProcessUntil(order, conditions) => self.execute_process_until(*order, conditions),
            ProcessUntilDecreasing(order, int_op, conditions) => {
                self.execute_process_until_decreasing(*order, int_op, conditions)
            }
            SearchAll(order, conditions) => self.execute_search_all(*order, conditions),
            Collate(constr, reps, op) => self.execute_collate(constr, op, *reps),
            Help => Self::print_help(),
        }
    }
}

impl fmt::Display for Controller {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Constructor: {}\nOperations: [{}]", self.instruction, self.operations_string())
    }
}
