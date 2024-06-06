use crate::cnf::section_vec::SectionVec;
use std::collections::{BTreeMap, BTreeSet};

pub(super) type Symbol = usize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Binding {
    rank: u32,
    parent: Symbol,
}

impl Binding {
    pub fn new(parent: Symbol) -> Self {
        Self { rank: 0, parent }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Bindings {
    bindings: BTreeMap<Symbol, Binding>,
}

impl Bindings {
    pub fn new() -> Self {
        Self {
            bindings: BTreeMap::new(),
        }
    }

    pub fn find_root(&self, symbol: Symbol) -> Symbol {
        let Some(bind) = self.bindings.get(&symbol) else {
            return symbol;
        };

        if bind.parent == symbol {
            return symbol;
        }

        self.find_root(bind.parent)
    }

    pub fn bind(&mut self, symbol_1: Symbol, symbol_2: Symbol) -> (Option<Symbol>, Symbol) {
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

    fn binding_mut(&mut self, symbol: Symbol) -> &mut Binding {
        let bind = self.bindings.entry(symbol).or_insert(Binding::new(symbol));

        if bind.parent == symbol {
            let binding_ptr = bind as *mut Binding;

            // A hack to work around the borrow checker not accepting the code
            // SAFETY: self is borrowed mutably, therefor this pointer is a valid reference
            return unsafe { &mut *binding_ptr };
        }

        let parent = bind.parent;
        self.binding_mut(parent)
    }

    pub fn into_section_vec(&self) -> SectionVec<Symbol> {
        let mut reverse_map: BTreeSet<(Symbol, Symbol)> = BTreeSet::new();

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

        section_vec
    }
}
