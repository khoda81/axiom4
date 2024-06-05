use std::{collections::HashMap, num::NonZeroU32, ops::Deref, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

impl Symbol {
    pub fn as_usize(self) -> usize {
        self.0.get() as usize
    }

    pub fn as_u64(self) -> u64 {
        self.0.get() as u64
    }

    pub fn from_usize(value: usize) -> Option<Self> {
        NonZeroU32::new(value as u32).map(Symbol)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringInterner {
    pool: Vec<Rc<str>>,
    // TODO: Try BTreeMap
    reverse_pool: HashMap<Rc<str>, Symbol>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            pool: vec![Rc::from("")],
            reverse_pool: HashMap::new(),
        }
    }

    pub fn find(&self, name: &str) -> Option<Symbol> {
        self.reverse_pool.get(name).copied()
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.pool.get(symbol.0.get() as usize).map(Rc::deref)
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&symbol) = self.reverse_pool.get(name) {
            return symbol;
        }

        let symbol_id = NonZeroU32::new(self.pool.len() as u32)
            .expect("the length of pool should not be 0 or exceed u32::MAX");

        let rc_name: Rc<str> = Rc::from(name);
        self.pool.push(rc_name.clone());
        self.reverse_pool.insert(rc_name, Symbol(symbol_id));
        Symbol(symbol_id)
    }

    pub fn pool(&self) -> &[Rc<str>] {
        &self.pool
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}
