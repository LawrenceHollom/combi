mod domination;
mod chromatic;
mod max_acyclic;
mod monotone;
mod bunkbed;
mod percolate;
mod bunkbed_posts;
mod cliques;
mod girth;
mod interval_coloring;
mod planar;
mod induced_forest;
mod connectedness;
mod cubic_domination;
mod subgraphs;
mod debug;
mod signature;
mod edge_partitions;

use std::collections::HashMap;

use utilities::rational::*;

use crate::annotations::*;
use crate::constructor::Constructor;
use crate::operation::*;
use crate::graph::*;

use crate::operation::bool_operation::*;
use crate::operation::rational_operation::*;
use crate::operation::int_operation::*;
use crate::operation::unit_operation::*;

pub struct Operator {
    g: Graph,
    previous_int_values: HashMap<IntOperation, u32>,
    previous_bool_values: HashMap<BoolOperation, bool>,
}

pub struct AnnotationsBox(Option<Annotations>);

/**
 * This stores optional graph annotations which can be filled in as and
 * when they are needed.  Possibly this should also take a hash of the
 * graph to ensure that the right annotations are always used for the
 * right graph.
 */
impl AnnotationsBox {
    pub fn new() -> AnnotationsBox {
        AnnotationsBox(None)
    }

    pub fn get_annotations(&mut self, g: &Graph) -> &mut Annotations {
        if self.0.is_none() {
            let ann = Annotations::new(g);
            self.0 = Some(ann);
        }
        self.0.as_mut().unwrap()
    }
}

impl Operator {
    fn operate_int(&mut self, ann: &mut AnnotationsBox, operation: &IntOperation) -> u32 {
        use IntOperation::*;
        match self.previous_int_values.get(operation) {
            Some(value) => *value,
            None => {
                let value = match operation {
                    Order => self.g.n.to_usize() as u32,
                    Size => self.g.size() as u32,
                    LargestComponent => self.g.largest_component(),
                    NumComponents => self.g.num_components(),
                    CliqueNumber => cliques::largest_clique(&self.g),
                    IndependenceNumber => cliques::independence_number(&self.g),
                    Girth => girth::girth(&self.g),
                    DominationNumber => domination::domination_number(&self.g),
                    EdgeDominationNumber => domination::edge_domination_number(&self.g),
                    ChromaticNumber => chromatic::chromatic_number(&self.g),
                    MaxAcyclicSubgraph => max_acyclic::max_acyclic_subgraph(&self.g),
                    CliqueCoveringNumber => chromatic::chromatic_number(&self.g.complement()),
                    NumOnLongMonotone => monotone::num_on_long_monotone(&self.g),
                    MaxMonotonePath => monotone::max_monotone(&self.g),
                    NumOnMonotoneCycle => monotone::num_on_monot_cycle(&self.g),
                    MaxFindableRigidComponent => monotone::max_rigid_component(&self.g),
                    TotalDominationGameLength => domination::total_domination_game_length(&self.g),
                    NumIntervalColors => interval_coloring::num_interval_colors(&self.g),
                    MaxInducedForest => induced_forest::max_induced_forest(&self.g),
                    MinDegree => self.g.min_degree(),
                    MaxDegree => self.g.max_degree(),
                    Connectedness => connectedness::connectedness(&self.g),
                    Diameter => self.g.diameter(),
                    Radius => self.g.radius(),
                    Thomassen(long_path_cap) => edge_partitions::thomassen_check(&self.g, *long_path_cap),
                    NumBipartiteEdgeBisections => edge_partitions::count_bipartite_edge_bisections(&self.g),
                    GameChromaticNumber => chromatic::game_chromatic_number(&self.g, ann.get_annotations(&self.g)),
                    GameChromaticNumberGreedy => chromatic::alice_greedy_lower_bound(&self.g) as u32,
                    Number(k) => *k,
                };
                self.previous_int_values.insert(*operation, value);
                value
            }
        }
    }
    
    fn operate_int_to_bool_infix(&mut self, ann: &mut AnnotationsBox, infix: &NumToBoolInfix, op1: &IntOperation, op2: &IntOperation) -> bool {
        use NumToBoolInfix::*;
        let x1 = self.operate_int(ann, op1);
        let x2 = self.operate_int(ann, op2);
        match *infix {
            Less => x1 < x2,
            More => x1 > x2,
            NotLess => x1 >= x2,
            NotMore => x1 <= x2,
            Equal => x1 == x2,
            NotEqual => x1 != x2,
        }
    }

    fn operate_float_to_bool_infix(&mut self, ann: &mut AnnotationsBox, infix: &NumToBoolInfix, op1: &RationalOperation, op2: &RationalOperation) -> bool {
        use NumToBoolInfix::*;
        let x1 = self.operate_rational(ann, op1);
        let x2 = self.operate_rational(ann, op2);
        match *infix {
            Less => x1 < x2,
            More => x1 > x2,
            NotLess => x1 >= x2,
            NotMore => x1 <= x2,
            Equal => x1 == x2,
            NotEqual => x1 != x2,
        }
    }
    
    fn operate_bool_to_bool_infix(&mut self, ann: &mut AnnotationsBox, infix: &BoolToBoolInfix, op1: &BoolOperation, op2: &BoolOperation) -> bool {
        use BoolToBoolInfix::*;
        let x1 = self.operate_bool(ann, op1);
        let x2 = self.operate_bool(ann, op2);
        match *infix {
            And => x1 && x2,
            Or => x1 || x2,
            Implies => !x1 || x2,
            Xor => x1 ^ x2,
        }
    }

    pub fn operate_bool(&mut self, ann: &mut AnnotationsBox, operation: &BoolOperation) -> bool {
        use BoolOperation::*;
        match self.previous_bool_values.get(operation) {
            Some(value) => *value,
            None => {
                let value = match operation {
                    IntInfix(infix, op1, op2) => {
                        self.operate_int_to_bool_infix(ann, infix, op1, op2)
                    }
                    FloatInfix(infix, op1, op2) => {
                        self.operate_float_to_bool_infix(ann, infix, op1, op2)
                    }
                    BoolInfix(infix, op1, op2) => {
                        self.operate_bool_to_bool_infix(ann, infix, op1, op2)
                    }
                    Not(op) => !self.operate_bool(ann, op),
                    IsDominationNumberAtLeast(lower_bound) => {
                        domination::is_domination_number_at_least(&self.g, *lower_bound)
                    }
                    Const(val) => *val,
                    IsConnected => self.g.is_connected(),
                    HasLongMonotone => monotone::has_long_monotone(&self.g),
                    HasIntervalColoring => interval_coloring::has_interval_coloring(&self.g),
                    IsPlanar => planar::is_planar(&self.g),
                    IsRegular => self.g.is_regular(),
                    BunkbedDiffsAllUnimodal => bunkbed::are_all_diffs_unimodal(&self.g),
                    HasRegularLosslessEdgeDominator => domination::has_regular_lossless_edge_dominator(&self.g),
                    IsKConnected(connectivity) => connectedness::is_k_connected(&self.g, *connectivity),
                    IsTriangleFree => subgraphs::is_triangle_free(&self.g),
                    CanBeEdgePartitionedIntoLinearForestAndMatching => edge_partitions::edge_partition_forest_and_matching(&self.g),
                    GoodCubicDominationBase => cubic_domination::is_good(&self.g),
                    GameChromaticWinner(k) => chromatic::alice_wins_chromatic_game(&self.g, ann.get_annotations(&self.g), *k),
                    IsChromaticGameMonotone => chromatic::game_chromatic_colour_monotone(&self.g, ann.get_annotations(&self.g)),
                    Debug => debug::debug(&self.g),
                };
                self.previous_bool_values.insert(operation.to_owned(), value);
                value
            }
        }
    }

    pub fn operate_rational(&mut self, ann: &mut AnnotationsBox, operation: &RationalOperation) -> Rational {
        use RationalOperation::*;
        match operation {
            OfBool(operation) => 
                if self.operate_bool(ann, operation) { Rational::ONE } else { Rational::ZERO },
            OfInt(operation) =>
                Rational::new(self.operate_int(ann, operation) as i64),
            Arithmetic(arith, op1, op2) => {
                let par1 = self.operate_rational(ann, op1);
                let par2 = self.operate_rational(ann, op2);
                use ArithmeticOperation::*;
                match arith {
                    Sum => par1 + par2,
                    Difference => par1 - par2,
                    Product => par1 * par2,
                    Ratio => par1 / par2,
                }
            }
            DominationRedundancy => domination::domination_redundancy(&self.g),
        }
    }

    fn operate_unit(&mut self, ann: &mut AnnotationsBox, operation: &UnitOperation) {
        use UnitOperation::*;
        match operation {
            Print => self.g.print(),
            RawBunkbed => bunkbed::print_polynomials(&self.g),
            BunkbedPosts => bunkbed_posts::print_polynomials(&self.g),
            BunkbedSimulation => bunkbed::simulate(&self.g),
            PercolationPolys => percolate::print_polynomials(&self.g),
            BunkbedCuts(u) => bunkbed::compute_problem_cuts(&self.g, *u),
            BunkbedDists => bunkbed::print_distance_polynomials(&self.g),
            BunkbedDiffs(u, print_size) => bunkbed::interesting_configurations(&self.g, *u, *print_size),
            PrintIntervalColoring => interval_coloring::print_interval_coloring(&self.g),
            PrintDominatingSet => domination::print_random_dominator(&self.g),
            GameChromaticTable => chromatic::print_game_chromatic_table(&self.g, ann.get_annotations(&self.g)),
            PrintAutomorphisms => print_automorphism_info(&self.g),
            Signature => signature::print_signature(&self.g),
            Unit => (),
        }
    }

    pub fn operate(&mut self, ann: &mut AnnotationsBox, operation: &Operation) -> String {
        use Operation::*;
        match operation {
            Int(op) => u32::to_string(&self.operate_int(ann, op)),
            Bool(op) => bool::to_string(&self.operate_bool(ann, op)),
            Rational(op) => format!("{:.4}", self.operate_rational(ann, op)),
            Unit(op) => {
                self.operate_unit(ann, op);
                "()".to_owned()
            }
        }
    }

    pub fn print_all(&self) {
        let mut bool_keys: Vec<&BoolOperation> = self.previous_bool_values.keys().collect();
        bool_keys.sort();
        for key in bool_keys.iter() {
            print!("({}: {}) ", *key, self.previous_bool_values.get(key).unwrap());
        }

        let mut int_keys: Vec<&IntOperation> = self.previous_int_values.keys().collect();
        int_keys.sort();
        for key in int_keys.iter() {
            print!("({}: {}) ", *key, self.previous_int_values.get(key).unwrap());
        }
        println!();
    }

    pub fn print_graph(&self) {
        self.g.print()
    }

    pub fn get_constructor(&self) -> &Constructor {
        &self.g.constructor
    }

    pub fn new(g: Graph) -> Operator {
        Operator { 
            g,
            previous_int_values: HashMap::new(),
            previous_bool_values: HashMap::new(),
        }
    }
}