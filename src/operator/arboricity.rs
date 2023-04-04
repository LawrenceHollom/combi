use utilities::edge_tools::*;

fn maker_wins_arboricity_game_rec(g: &Graph, indexer: &EdgeIndexer, k: usize, max_colour_used: usize, 
        colours: EdgeVec<Option<usize>>, num_edges: usize, num_cold: usize) -> bool {
    // Recursively try to colour an edge.
    if num_cold >= num_edges {
        // maker wins as all edges are coloured.
        true
    } else {
        // Colour another edge if possible.
        let mut is_something_playable = false;
        'test_edges: for e in indexer.iter_edges() {
            
        }
    }
}

fn maker_wins_arboricity_game(g: &Graph, k: usize) -> bool {

}

fn game_arboricity_number(g: &Graph) -> u32 {
    let mut k = 1;
    loop {
        if maker_wins_arboricity_game(g, k) {
            return k as u32;
        }
        k += 1;
    }
}