use crate::cnf::section_vec::SectionVec;
use std::collections::{BTreeMap, BTreeSet};

pub(super) type Var = usize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Bucket {
    rank: u32,
    parent: Var,
}

impl Bucket {
    pub fn new(parent: Var) -> Self {
        Self { rank: 0, parent }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Buckets {
    bindings: BTreeMap<Var, Bucket>,
}

impl Buckets {
    pub fn new() -> Self {
        Self {
            bindings: BTreeMap::new(),
        }
    }

    pub fn find_root(&self, symbol: Var) -> Var {
        let Some(bind) = self.bindings.get(&symbol) else {
            return symbol;
        };

        if bind.parent == symbol {
            return symbol;
        }

        self.find_root(bind.parent)
    }

    pub fn merge(&mut self, symbol_1: Var, symbol_2: Var) -> (Option<Var>, Var) {
        let bind_2 = *self.binding_mut(symbol_2);
        let bind_1 = self.binding_mut(symbol_1);

        let root_1 = bind_1.parent;
        let root_2 = bind_2.parent;

        if root_1 == root_2 {
            // Do nothing
            (None, root_2)
        } else if bind_1.rank < bind_2.rank {
            bind_1.parent = root_2;
            (Some(root_1), root_2)
        } else {
            if bind_1.rank == bind_2.rank {
                bind_1.rank += 1;
            }

            if let Some(b) = self.bindings.get_mut(&root_2) {
                b.parent = root_1;
            }

            (Some(root_2), root_1)
        }
    }

    fn binding_mut(&mut self, symbol: Var) -> &mut Bucket {
        let bind = self.bindings.entry(symbol).or_insert(Bucket::new(symbol));

        if bind.parent == symbol {
            let binding_ptr = bind as *mut Bucket;

            // A hack to work around the borrow checker not accepting the code
            // SAFETY: self is borrowed mutably, therefor this pointer is a valid reference
            return unsafe { &mut *binding_ptr };
        }

        let parent = bind.parent;
        self.binding_mut(parent)
    }

    pub fn into_section_vec(&self) -> SectionVec<Var> {
        let mut reverse_map: BTreeSet<(Var, Var)> = BTreeSet::new();

        for &symbol in self.bindings.keys() {
            let root = self.find_root(symbol);
            reverse_map.insert((root, symbol));
        }

        let mut section_vec = SectionVec::new();
        let mut items = reverse_map.into_iter();

        let Some((mut current_root, item)) = items.next() else {
            return section_vec;
        };

        section_vec.push(item);

        for (root, item) in items {
            if root != current_root {
                section_vec.push_section();
            }

            current_root = root;
            section_vec.push(item);
        }

        section_vec.push_section();

        section_vec
    }
}
