use std::fmt;

#[derive(Eq, Hash, PartialEq, Copy, Clone, Debug)]
pub enum IntOperation {
    Order,
    Size,
    LargestComponent,
    NumComponents,
    CliqueNumber,
    IndependenceNumber,
    Girth,
    DominationNumber,
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
    Number(u32),
}

impl IntOperation {
    pub fn of_string_result(text: &str) -> Option<IntOperation> {
        use IntOperation::*;
        match text.trim().trim_end_matches(')').to_lowercase().as_str() {
            "order" | "n" => Some(Order),
            "size" | "edges" => Some(Size),
            "largest_component" | "largest" => Some(LargestComponent),
            "num_components" | "comps" => Some(NumComponents),
            "largest_clique" | "clique" | "omega" => Some(CliqueNumber),
            "independence" | "indep" | "alpha" => Some(IndependenceNumber),
            "girth" => Some(Girth),
            "domination" | "dominator" | "gamma" => Some(DominationNumber),
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
            Number(n) => {
                sta = n.to_string();
                sta.as_str()
            }
        };
        write!(f, "{}", name)
    }
}