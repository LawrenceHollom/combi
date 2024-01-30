use super::reduced_equivalence_relation::*;

#[derive(Copy, Clone)]
pub enum EdgeType {
	Classic,
	FiftyFifty,
	Double,
	ThreeWay,
	Posted,
}

impl EdgeType {
	pub fn of_usize(edge_type: usize) -> EdgeType {
		use EdgeType::*;
		match edge_type {
			0 => Classic,
			1 => FiftyFifty,
			2 => Double,
			3 => ThreeWay,
			4 => Posted,
			_ => FiftyFifty,
		}
	}
	
	pub fn to_rer_vec(&self) -> Vec<ReducedEquivalenceRelation> {
		use EdgeType::*;
		let raw_edges = match self {
			Classic => vec![[[0, 0], [1, 2]], [[1, 2], [0, 0]], [[0, 1], [2, 3]], [[0, 0], [0, 0]]],
			FiftyFifty => vec![[[0, 0], [1, 2]], [[1, 2], [0, 0]]],
			Double => vec![[[0, 1], [1, 0]], [[0, 0], [1, 1]]],
			ThreeWay => vec![[[0, 0], [1, 1]], [[0, 0], [1, 1]], [[1, 2], [0, 1]], [[1, 0], [0, 2]]],
			Posted => vec![[[0, 0], [1, 2]], [[0, 2], [1, 1]], [[1, 2], [0, 1]], [[1, 0], [0, 2]]],
		};
		raw_edges.iter()
			.map(|raw| ReducedEquivalenceRelation::of_raw_vecs(raw)
				.reduce_no_symmetrise(true))
			.collect::<Vec<ReducedEquivalenceRelation>>()
	}
}