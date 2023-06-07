use std::fmt;

use utilities::parse_function_like;

#[derive(Eq, Hash, PartialEq, Copy, Clone, Debug, PartialOrd, Ord)]
pub enum IntOperation {
    Order,
    Size,
    LargestComponent,
    NumComponents,
    CliqueNumber,
    IndependenceNumber,
    Girth,
    DominationNumber,
    EdgeDominationNumber,
    ChromaticNumber,
    MaxAcyclicSubgraph,
    CliqueCoveringNumber,
    NumOnLongMonotone,
    MaxMonotonePath,
    NumOnMonotoneCycle,
    MaxFindableRigidComponent,
    TotalDominationGameLength,
    NumIntervalColors,
    MaxInducedForest,
    MinDegree,
    MaxDegree,
    Connectedness,
    Diameter,
    Radius,
    Thomassen(Option<usize>),
    NumBipartiteEdgeBisections,
    GameChromaticNumber,
    GameChromaticNumberGreedy,
    GameArboricityNumber,
    Degeneracy,
    LinearGameChromaticNumber,
    GameGrundyNumber,
    MarkingGameNumber,
    Number(u32),
}

impl IntOperation {
    pub fn of_string_result(text: &str) -> Option<IntOperation> {
        use IntOperation::*;
        let (func, args) = parse_function_like(text);
        match func.trim().trim_end_matches(')').to_lowercase().as_str() {
            "order" | "n" => Some(Order),
            "size" | "edges" => Some(Size),
            "largest_component" | "largest" => Some(LargestComponent),
            "num_components" | "comps" => Some(NumComponents),
            "largest_clique" | "clique" | "omega" => Some(CliqueNumber),
            "independence" | "indep" | "alpha" => Some(IndependenceNumber),
            "girth" => Some(Girth),
            "domination" | "dominator" | "gamma" => Some(DominationNumber),
            "edge_domination" | "edge_dominator" | "gamma_e" => Some(EdgeDominationNumber),
            "chromatic" | "chi" => Some(ChromaticNumber),
            "max_acyclic" | "acyclic" => Some(MaxAcyclicSubgraph),
            "clique_cover" | "theta" => Some(CliqueCoveringNumber),
            "num_on_long_monotone" | "num_monot" => Some(NumOnLongMonotone),
            "max_monot" => Some(MaxMonotonePath),
            "num_monot_cycle" => Some(NumOnMonotoneCycle),
            "max_rigid" => Some(MaxFindableRigidComponent),
            "total_domination_game" | "gamma_tg" => Some(TotalDominationGameLength),
            "num_interval_colors" => Some(NumIntervalColors),
            "max_induced_forest" | "max_forest" => Some(MaxInducedForest),
            "min_degree" | "min_deg" => Some(MinDegree),
            "max_degree" | "max_deg" | "delta" => Some(MaxDegree),
            "connectedness" | "connectivity" => Some(Connectedness),
            "diameter" | "diam" => Some(Diameter),
            "radius" => Some(Radius),
            "thomassen" => {
                if args.is_empty() {
                    Some(Thomassen(None))
                } else {
                    Some(Thomassen(Some(args[0].parse().unwrap())))
                }
            }
            "bipartite_edge_bisections" | "beb" => Some(NumBipartiteEdgeBisections),
            "game_chromatic_number" | "chi_g" => Some(GameChromaticNumber),
            "game_chromatic_alice_greedy" | "chi_g_greedy" => Some(GameChromaticNumberGreedy),
            "game_arboricity_number" | "a_g" => Some(GameArboricityNumber),
            "degeneracy" | "degen" => Some(Degeneracy),
            "linear_chi_g" | "lcg" => Some(LinearGameChromaticNumber),
            "game_grundy_number" | "game_grundy" => Some(GameGrundyNumber),
            "marking_game_number" | "mark" => Some(MarkingGameNumber),
            str => str.parse().ok().map(Number),
        }
    }
}

impl fmt::Display for IntOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IntOperation::*;
        let sta;
        let name = match self {
            Order => "Order",
            Size => "Size",
            LargestComponent => "Largest component",
            NumComponents => "Number of components",
            CliqueNumber => "Largest clique",
            IndependenceNumber => "Independence number",
            Girth => "Girth",
            DominationNumber => "Domination number",
            EdgeDominationNumber => "Edge domination number",
            ChromaticNumber => "Chromatic number",
            MaxAcyclicSubgraph => "Max acyclic subgraph size",
            CliqueCoveringNumber => "Clique covering number",
            NumOnLongMonotone => "Number of vertices on a 1->n monotone path",
            MaxMonotonePath => "Length of longest monotone path",
            NumOnMonotoneCycle => "Number of vertices on a monotone cycle",
            MaxFindableRigidComponent => "Max rigid component via monot cycles",
            TotalDominationGameLength => "Length of the total domination game",
            NumIntervalColors => "Number of colors in the first found interval coloring",
            MaxInducedForest => "Order of largest induced forest",
            MinDegree => "Min degree",
            MaxDegree => "Max degree",
            Connectedness => "Connectedness",
            Diameter => "Diameter",
            Radius => "Radius",
            Thomassen(_) => "Minimax path length in linear forest bisection of cubic graph",
            NumBipartiteEdgeBisections => "Number of 2-edge-colourings of cubic graph with both parts bipartite",
            GameChromaticNumber => "Game chromatic number",
            GameChromaticNumberGreedy => "Threshold for Alice to win greedily in chi_g",
            GameArboricityNumber => "Game arboricity number",
            Degeneracy => "Degeneracy",
            LinearGameChromaticNumber => "Linear game chromatic number",
            GameGrundyNumber => "Game Grundy number",
            MarkingGameNumber => "marking game number",
            Number(n) => {
                sta = n.to_string();
                sta.as_str()
            }
        };
        write!(f, "{}", name)
    }
}