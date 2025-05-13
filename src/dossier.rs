mod arboricity;
mod bunkbed;
mod bunkbed_posts;
mod bunkbed_reduced;
mod bunkbed_sites;
mod chromatic;
mod chromatic_linear;
mod cliques;
mod connectedness;
mod cubic_domination;
mod debug;
mod degeneracy;
mod directed;
mod domination;
mod edge_partitions;
mod girth;
mod grabbing;
mod grundy;
mod hyperbolic;
mod induced_forest;
mod interval_coloring;
mod kernels;
mod kozma_nitzan;
mod kuramoto;
mod marking_game;
mod matching;
mod max_acyclic;
mod monotone;
mod norine;
mod percolate;
mod planar;
mod poset_balance;
mod pretty;
mod seymour;
mod signature;
mod subgraphs;
mod twins;

use std::collections::HashMap;

use utilities::rational::*;

use crate::annotations::*;
use crate::constructor::Constructor;
use crate::entity::graph::*;
use crate::entity::*;
use crate::operation::*;

use crate::operation::bool_operation::*;
use crate::operation::int_operation::*;
use crate::operation::rational_operation::*;
use crate::operation::string_list_operation::*;
use crate::operation::unit_operation::*;

pub struct Dossier {
    e: Entity,
    previous_int_values: HashMap<IntOperation, u32>,
    previous_bool_values: HashMap<BoolOperation, bool>,
    previous_rational_values: HashMap<RationalOperation, Rational>,
}

pub struct AnnotationsBox(Option<Annotations>);

/**
 * This stores optional graph annotations which can be filled in as and
 * when they are needed.  Possibly this should also take a hash of the
 * graph to ensure that the right annotations are always used for the
 * right graph.
 */
impl AnnotationsBox {
    pub fn new() -> Self {
        Self(None)
    }

    pub fn get_annotations(&mut self, g: &Graph) -> &mut Annotations {
        if self.0.is_none() {
            let ann = Annotations::new(g);
            self.0 = Some(ann);
        }
        self.0.as_mut().unwrap()
    }
}

impl Dossier {
    fn unbox<'a>(&self, ann_box: &'a mut AnnotationsBox) -> &'a mut Annotations {
        ann_box.get_annotations(self.e.as_graph())
    }

    pub fn operate_int(&mut self, ann_box: &mut AnnotationsBox, operation: &IntOperation) -> u32 {
        use IntOperation::*;
        match self.previous_int_values.get(operation) {
            Some(value) => *value,
            None => {
                let value = match operation {
                    Order => self.e.order().to_usize() as u32,
                    Size => self.e.as_graph().size() as u32,
                    Height => self.e.as_poset().height as u32,
                    Width => self.e.as_poset().get_width(),
                    LargestComponent => self.e.as_graph().largest_component(),
                    NumComponents => self.e.as_graph().num_components(),
                    CliqueNumber => cliques::largest_clique(self.e.as_graph()),
                    IndependenceNumber => cliques::independence_number(self.e.as_graph()),
                    Girth => girth::girth(&self.e),
                    DominationNumber => domination::domination_number(self.e.as_graph()),
                    EdgeDominationNumber => domination::edge_domination_number(self.e.as_graph()),
                    ChromaticNumber => chromatic::chromatic_number(self.e.as_graph()),
                    MaxAcyclicSubgraph => max_acyclic::max_acyclic_subgraph(self.e.as_graph()),
                    CliqueCoveringNumber => {
                        chromatic::chromatic_number(&self.e.as_graph().complement())
                    }
                    NumOnLongMonotone => monotone::num_on_long_monotone(self.e.as_graph()),
                    MaxMonotonePath => monotone::max_monotone(self.e.as_graph()),
                    NumOnMonotoneCycle => monotone::num_on_monot_cycle(self.e.as_graph()),
                    MaxFindableRigidComponent => monotone::max_rigid_component(self.e.as_graph()),
                    TotalDominationGameLength => {
                        domination::total_domination_game_length(self.e.as_graph())
                    }
                    NumIntervalColors => interval_coloring::num_interval_colors(self.e.as_graph()),
                    MaxInducedForest => induced_forest::max_induced_forest(self.e.as_graph()),
                    MinDegree => self.e.as_graph().min_degree().to_usize() as u32,
                    MaxDegree => self.e.as_graph().max_degree().to_usize() as u32,
                    Connectedness => connectedness::connectedness(self.e.as_graph()),
                    Diameter => self.e.as_graph().diameter(),
                    Radius => self.e.as_graph().radius(),
                    Thomassen(long_path_cap) => {
                        edge_partitions::thomassen_check(self.e.as_graph(), *long_path_cap)
                    }
                    NumBipartiteEdgeBisections => {
                        edge_partitions::count_bipartite_edge_bisections(self.e.as_graph())
                    }
                    GameChromaticNumber => {
                        chromatic::game_chromatic_number(self.e.as_graph(), self.unbox(ann_box))
                    }
                    GameChromaticNumberGreedy => {
                        chromatic::alice_greedy_lower_bound(self.e.as_graph()) as u32
                    }
                    GameArboricityNumber => arboricity::game_arboricity_number(self.e.as_graph()),
                    Degeneracy => degeneracy::degeneracy(self.e.as_graph()),
                    LinearGameChromaticNumber => {
                        chromatic_linear::linear_game_chromatic_number(self.e.as_graph())
                    }
                    GameGrundyNumber => {
                        grundy::game_grundy_number(self.e.as_graph(), self.unbox(ann_box))
                    }
                    MarkingGameNumber => {
                        marking_game::marking_game_number(self.e.as_graph(), false)
                    }
                    ConnectedGameChromaticNumber => chromatic::connected_game_chromatic_number(
                        self.e.as_graph(),
                        self.unbox(ann_box),
                    ),
                    ConnectedMarkingGameNumber => {
                        marking_game::marking_game_number(self.e.as_graph(), true)
                    }
                    BipartiteSideDifference => {
                        chromatic::bipartite_side_difference(self.e.as_graph())
                    }
                    NumCutvertices => connectedness::num_cutvertices(self.e.as_graph()),
                    NumLinearExtensions => poset_balance::num_linear_extensions(self.e.as_poset()),
                    MaxMatching => matching::max_matching_size(self.e.as_graph()),
                    MinNorineDistance(colouring_type) => {
                        norine::min_antipode_distance(self.e.as_graph(), *colouring_type)
                    }
                    MinKernelSize => kernels::min_kernel_size(self.e.as_digraph(), false),
                    MaxCodegree => self.e.as_graph().max_codegree() as u32,
                    Number(k) => *k,
                };
                self.previous_int_values.insert(*operation, value);
                value
            }
        }
    }

    fn operate_int_to_bool_infix(
        &mut self,
        ann: &mut AnnotationsBox,
        infix: &NumToBoolInfix,
        op1: &IntOperation,
        op2: &IntOperation,
    ) -> bool {
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

    fn operate_float_to_bool_infix(
        &mut self,
        ann: &mut AnnotationsBox,
        infix: &NumToBoolInfix,
        op1: &RationalOperation,
        op2: &RationalOperation,
    ) -> bool {
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

    fn operate_bool_to_bool_infix(
        &mut self,
        ann: &mut AnnotationsBox,
        infix: &BoolToBoolInfix,
        op1: &BoolOperation,
        op2: &BoolOperation,
    ) -> bool {
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

    pub fn operate_bool(
        &mut self,
        ann_box: &mut AnnotationsBox,
        operation: &BoolOperation,
    ) -> bool {
        use BoolOperation::*;
        match self.previous_bool_values.get(operation) {
            Some(value) => *value,
            None => {
                let value = match operation {
                    IntInfix(infix, op1, op2) => {
                        self.operate_int_to_bool_infix(ann_box, infix, op1, op2)
                    }
                    FloatInfix(infix, op1, op2) => {
                        self.operate_float_to_bool_infix(ann_box, infix, op1, op2)
                    }
                    BoolInfix(infix, op1, op2) => {
                        self.operate_bool_to_bool_infix(ann_box, infix, op1, op2)
                    }
                    Not(op) => !self.operate_bool(ann_box, op),
                    IsDominationNumberAtLeast(lower_bound) => {
                        domination::is_domination_number_at_least(self.e.as_graph(), *lower_bound)
                    }
                    Const(val) => *val,
                    IsConnected => self.e.is_connected(),
                    HasLongMonotone => monotone::has_long_monotone(self.e.as_graph()),
                    HasIntervalColoring => {
                        interval_coloring::has_interval_coloring(self.e.as_graph())
                    }
                    IsPlanar => planar::is_planar(self.e.as_graph()),
                    IsRegular => self.e.as_graph().is_regular(),
                    BunkbedDiffsAllUnimodal => bunkbed::are_all_diffs_unimodal(self.e.as_graph()),
                    HasRegularLosslessEdgeDominator => {
                        domination::has_regular_lossless_edge_dominator(self.e.as_graph())
                    }
                    IsKConnected(connectivity) => {
                        connectedness::is_k_connected(self.e.as_graph(), *connectivity)
                    }
                    IsTriangleFree => subgraphs::is_triangle_free(self.e.as_graph()),
                    CanBeEdgePartitionedIntoLinearForestAndMatching => {
                        edge_partitions::edge_partition_forest_and_matching(self.e.as_graph())
                    }
                    GoodCubicDominationBase => cubic_domination::is_good(self.e.as_graph()),
                    GameChromaticWinner(k) => chromatic::maker_wins_chromatic_game(
                        self.e.as_graph(),
                        self.unbox(ann_box),
                        *k,
                        false,
                        false,
                    ),
                    IsChromaticGameMonotone => chromatic::game_chromatic_colour_monotone(
                        self.e.as_graph(),
                        self.unbox(ann_box),
                    ),
                    IsChromaticGameStronglyMonotone => {
                        chromatic::chromatic_game_strong_monotonicity(
                            self.e.as_graph(),
                            self.unbox(ann_box),
                        )
                    }
                    ArboricityGameWinner(k) => {
                        arboricity::maker_wins_arboricity_game(self.e.as_graph(), *k)
                    }
                    IsDDegenerate(d) => degeneracy::is_d_degenerate(self.e.as_graph(), *d),
                    IsBunkbedPostRemovalInductionGood => {
                        bunkbed_reduced::is_post_removal_induction_always_good(self.e.as_graph())
                    }
                    CanChromaticGameHaveDudUniqueWin => {
                        chromatic::can_chormatic_game_dud_unique_win(
                            self.e.as_graph(),
                            self.unbox(ann_box),
                        )
                    }
                    GameChromaticWinnerWithDuds(k) => {
                        chromatic::alice_wins_chromatic_game_with_duds(
                            self.e.as_graph(),
                            self.unbox(ann_box),
                            *k,
                        )
                    }
                    ChromaticGameSubsetMonotonicity(k) => chromatic::is_some_subset_non_monotone(
                        self.e.as_graph(),
                        self.unbox(ann_box),
                        *k,
                    ),
                    LinearGameChromaticWinner(k) => {
                        chromatic_linear::does_maker_win_linear_chromatic_game(
                            self.e.as_graph(),
                            *k,
                        )
                    }
                    GameGrundyWinner(k) => grundy::does_maker_win_grundy_game(
                        self.e.as_graph(),
                        self.unbox(ann_box),
                        *k,
                    ),
                    MakerWinsConnectedChromaticGame(k) => chromatic::maker_wins_chromatic_game(
                        self.e.as_graph(),
                        self.unbox(ann_box),
                        *k,
                        true,
                        false,
                    ),
                    IsBipartite => chromatic::is_bipartite(self.e.as_graph()),
                    MakerWinsChromaticGameGreedily(k) => {
                        chromatic::alice_greedy_wins_chromatic_game(
                            self.e.as_graph(),
                            self.unbox(ann_box),
                            *k,
                        )
                    }
                    HasSeymourVertex => seymour::has_seymour_vertex(self.e.as_graph()),
                    CanBobWinGraphGrabbing(max_weight) => {
                        grabbing::can_bob_win_graph_grabbing(self.e.as_graph(), *max_weight)
                    }
                    IsOddCoronaFree => !grabbing::has_induced_odd_cycle_corona(self.e.as_graph()),
                    IsOddSemicoronaFree => {
                        !grabbing::has_induced_odd_cycle_semicorona(self.e.as_graph())
                    }
                    IsForkFree => !grabbing::has_induced_fork(self.e.as_graph()),
                    BunkbedSiteHasBadConditioning => {
                        bunkbed_sites::has_bad_conditioning(self.e.as_graph())
                    }
                    ContradictsBunkbedSiteConjecture => {
                        bunkbed_sites::contradicts_bb_site_conjecture(self.e.as_graph())
                    }
                    ContradictsReducedBunkbedConjecture => {
                        bunkbed_reduced::contradicts_reduced_bunkbed_conjecture(self.e.as_graph())
                    }
                    ApproxContradictsReducedBunkbedConjecture(samples) => {
                        bunkbed_reduced::approx_contradicts_reduced_bunkbed_conjecture(
                            self.e.as_graph(),
                            *samples,
                        )
                    }
                    ContradictsReducedConditionedBunkbedConjecture => {
                        bunkbed_reduced::contradicts_reduced_conditioned_bunkbed_conjecture(
                            self.e.as_graph(),
                        )
                    }
                    IsBunkbedReducible => bunkbed_reduced::is_bunkbed_reducible(self.e.as_graph()),
                    BunkbedGadget => bunkbed_reduced::is_contradictory_3_gadget(self.e.as_graph()),
                    IsPosetIncomparabilityConnected => {
                        self.e.as_poset().is_incomparability_connected()
                    }
                    HasTwinElements => twins::has_twin_elements(&self.e),
                    HasAlmostTwinElements => twins::has_almost_twin_elements(self.e.as_poset()),
                    HasAlmostTwinElementsIgnoreMaximal => {
                        twins::has_almost_twin_elements_exclude_maximal(self.e.as_poset())
                    }
                    NumLinearExtensionsLessThan(k) => {
                        poset_balance::is_num_extensions_less_than(self.e.as_poset(), *k)
                    }
                    IsHeuristicallyBalanced => {
                        poset_balance::is_heuristically_balanced(self.e.as_poset())
                    }
                    HasMaximalN => poset_balance::has_maximal_n(self.e.as_poset()),
                    IsForest => self.e.as_graph().is_forest(),
                    HasSource => self.e.as_digraph().has_source(),
                    HasSink => self.e.as_digraph().has_sink(),
                    HasBasin => directed::flow::has_global_basin(self.e.as_digraph()),
                    HasKernelOfSizeAtMost(size) => {
                        kernels::has_kernel_of_size_at_most(self.e.as_digraph(), *size)
                    }
                    KuramotoSynchronises(k) => {
                        kuramoto::does_random_config_synchronise(self.e.as_graph(), *k)
                    }
                    KuramotoSimplest(k) => {
                        kuramoto::find_simplest_unsynchronised(self.e.as_graph(), *k)
                    }
                    KuramotoCyclic => kuramoto::does_cyclic_config_synchronise(self.e.as_graph()),
                    IsKozmaNitzanFalse => {
                        kozma_nitzan::does_contradict_kozma_nitzan(self.e.as_graph())
                    }
                    IsSiteKozmaNitzanFalse => {
                        kozma_nitzan::does_contradict_site_kozma_nitzan(self.e.as_graph())
                    }
                    IsEveryEdgeWitnessed => {
                        directed::cycles::is_every_edge_witnessed(self.e.as_digraph())
                    }
                    HasBidirectionalEdge => {
                        directed::cycles::has_bidirectional_edge(self.e.as_digraph())
                    }
                    HasFourCycle => self.e.as_graph().contains_four_cycle(),
                    IsGenericallyHyperbolicEmbeddable(debug) => {
                        hyperbolic::does_embed_generically(self.e.as_graph(), *debug)
                    }
                    HasFewEdgesHyperbolic => {
                        hyperbolic::has_hereditarily_few_edges(self.e.as_graph())
                    }
                    Debug => debug::debug(self.e.as_graph()),
                };
                self.previous_bool_values
                    .insert(operation.to_owned(), value);
                value
            }
        }
    }

    pub fn operate_rational(
        &mut self,
        ann: &mut AnnotationsBox,
        operation: &RationalOperation,
    ) -> Rational {
        use RationalOperation::*;
        match self.previous_rational_values.get(operation) {
            Some(value) => *value,
            None => {
                let value = match operation {
                    OfBool(operation) => {
                        if self.operate_bool(ann, operation) {
                            Rational::ONE
                        } else {
                            Rational::ZERO
                        }
                    }
                    OfInt(operation) => Rational::new(self.operate_int(ann, operation) as i64),
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
                    DominationRedundancy => domination::domination_redundancy(self.e.as_graph()),
                    GrabbingColeafWeightedDifference => {
                        grabbing::coleaf_weighted_score_difference(self.e.as_graph())
                    }
                    PosetBalance => poset_balance::balance_constant(self.e.as_poset()),
                    PosetCapBalance => poset_balance::balance_as_cap(self.e.as_poset()),
                    PosetBalanceWithMinimal => poset_balance::balance_with_min(self.e.as_poset()),
                    NorineAverageDistance(colouring_type) => {
                        norine::average_antipode_distance(self.e.as_graph(), *colouring_type)
                    }
                };
                self.previous_rational_values
                    .insert(operation.to_owned(), value);
                value
            }
        }
    }

    fn operate_unit(&mut self, ann: &mut AnnotationsBox, operation: &UnitOperation) {
        use UnitOperation::*;
        match operation {
            Print => self.e.print(),
            PrintMatrix => self.e.as_graph().print_matrix(true, true),
            RawBunkbed => bunkbed::print_polynomials(self.e.as_graph()),
            BunkbedPosts => bunkbed_posts::print_polynomials(self.e.as_graph()),
            BunkbedSimulation => bunkbed::simulate(self.e.as_graph()),
            PercolationPolys => percolate::print_polynomials(self.e.as_graph()),
            BunkbedCuts(u) => bunkbed::compute_problem_cuts(self.e.as_graph(), *u),
            BunkbedDists => bunkbed::print_distance_polynomials(self.e.as_graph()),
            BunkbedDiffs(u, print_size) => {
                bunkbed::interesting_configurations(self.e.as_graph(), *u, *print_size)
            }
            PrintIntervalColoring => interval_coloring::print_interval_coloring(self.e.as_graph()),
            PrintDominatingSet => domination::print_random_dominator(self.e.as_graph()),
            GameChromaticTable => chromatic::print_game_chromatic_table(
                self.e.as_graph(),
                ann.get_annotations(self.e.as_graph()),
            ),
            GameChromaticStrategy(k) => chromatic::print_chromatic_game_strategy(
                self.e.as_graph(),
                ann.get_annotations(self.e.as_graph()),
                *k,
                false,
            ),
            ConnectedGameChromaticStrategy(k) => chromatic::print_chromatic_game_strategy(
                self.e.as_graph(),
                ann.get_annotations(self.e.as_graph()),
                *k,
                true,
            ),
            PrintAutomorphisms => print_automorphism_info(self.e.as_graph()),
            BunkbedPostRemoval => bunkbed_reduced::test_post_removal_induction(self.e.as_graph()),
            Signature => signature::print_signature(&self.e),
            PrintMarkingGameStrat => {
                marking_game::print_marking_game_strat(self.e.as_graph(), false)
            }
            PrintConnectedMarkingGameStrat => {
                marking_game::print_marking_game_strat(self.e.as_graph(), true)
            }
            PrintBobWinGrabbingWeighting => grabbing::print_bob_win_weighting(self.e.as_graph()),
            GrabbingHypothesisTest => grabbing::hypothesis_testing(self.e.as_graph()),
            BunkbedSiteCOunts => bunkbed_sites::print_counts(self.e.as_graph()),
            BunkbedReducedConnectionCounts(k) => {
                bunkbed_reduced::print_connection_counts(self.e.as_graph(), *k)
            }
            BunkbedReducedConnectionSimulation(num_reps, k) => {
                bunkbed_reduced::simulate_connection_count_ratios_naive(
                    self.e.as_graph(),
                    *num_reps,
                    *k,
                )
            }
            BunkbedReducedConnectionDP(num_reps, k, edge_type) => {
                bunkbed_reduced::bunkbed_connection_counts_dp(
                    self.e.as_graph(),
                    *num_reps,
                    *k,
                    *edge_type,
                )
            }
            BunkbedCounterexampleSearch(edge_type) => {
                bunkbed_reduced::search_for_counterexample(self.e.as_graph(), *edge_type)
            }
            PosetRelationProbabilities => {
                poset_balance::print_relation_probabilities(self.e.as_poset())
            }
            PosetHasseDiagram => self.e.as_poset().print_hasse(),
            BalancedHeuristics => poset_balance::print_heuristics(self.e.as_poset()),
            PosetPrintBalanceAsCap => poset_balance::print_cap_balance(self.e.as_poset()),
            PrintNorineHypercubeColourings => norine::partition_colourings(self.e.as_graph()),
            PrintSymmetricNorineHypercubeColourings(slice, out_of) => {
                norine::partition_symmetric_colourings(self.e.as_graph(), *slice, *out_of)
            }
            PrintMinKernel => {
                let _ = kernels::min_kernel_size(self.e.as_digraph(), true);
            }
            SimulateKuramoto => kuramoto::simulate(self.e.as_graph()),
            PrettyPrint => pretty::print_entity(&self.e),
            PrettyPrintCyclic => pretty::print_entity_cyclic(&self.e),
            PrintKozmaNitzan => kozma_nitzan::print(self.e.as_graph()),
            PrintSiteKozmaNitzan => kozma_nitzan::print_site(self.e.as_graph()),
            Unit => (),
        }
    }

    pub fn operate_string_list(
        &mut self,
        _ann: &mut AnnotationsBox,
        operation: &StringListOperation,
    ) -> Vec<String> {
        use StringListOperation::*;
        match operation {
            BunkbedSiteSignatures => bunkbed_sites::signatures(self.e.as_graph()),
            Serialise => vec![self.e.as_graph().serialise()],
        }
    }

    pub fn operate(&mut self, ann: &mut AnnotationsBox, operation: &Operation) -> String {
        use Operation::*;
        match operation {
            Int(op) => u32::to_string(&self.operate_int(ann, op)),
            Bool(op) => bool::to_string(&self.operate_bool(ann, op)),
            Rational(op) => format!("{}", self.operate_rational(ann, op)),
            Unit(op) => {
                self.operate_unit(ann, op);
                "()".to_owned()
            }
            StringList(op) => format!("{:?}", self.operate_string_list(ann, op)),
        }
    }

    pub fn print_all(&self) {
        let mut bool_keys: Vec<&BoolOperation> = self.previous_bool_values.keys().collect();
        bool_keys.sort();
        for key in bool_keys.iter() {
            if !key.is_recursive() {
                print!(
                    "({}: {}) ",
                    *key,
                    self.previous_bool_values.get(key).unwrap()
                );
            }
        }

        let mut int_keys: Vec<&IntOperation> = self.previous_int_values.keys().collect();
        int_keys.sort();
        for key in int_keys.iter() {
            print!(
                "({}: {}) ",
                *key,
                self.previous_int_values.get(key).unwrap()
            );
        }

        let mut rational_keys: Vec<&RationalOperation> =
            self.previous_rational_values.keys().collect();
        rational_keys.sort();
        for key in rational_keys.iter() {
            if !key.is_recursive() {
                print!(
                    "({}: {}) ",
                    *key,
                    self.previous_rational_values.get(key).unwrap()
                )
            }
        }
        println!();
    }

    pub fn print_entity(&self) {
        self.e.print()
    }

    pub fn get_constructor(&self) -> &Constructor {
        &self.e.as_graph().constructor
    }

    pub fn new(e: Entity) -> Self {
        Self {
            e,
            previous_int_values: HashMap::new(),
            previous_bool_values: HashMap::new(),
            previous_rational_values: HashMap::new(),
        }
    }
}
