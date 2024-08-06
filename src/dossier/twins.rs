use crate::entity::*;
use crate::entity::poset::*;

fn poset_has_twin_elements(p: &Poset) -> bool {

}

pub fn has_twin_elements(e: &Entity) -> bool {
    use Entity::*;
    match e {
        Graph(_) => panic!("You haven't coded this yet!"),
        Digraph(_) => panic!("You haven't coded this yet!"),
        Poset(p) => poset_has_twin_elements(p),
    }
}

pub fn has_almost_twin_elements(p: &Poset) -> bool {

}