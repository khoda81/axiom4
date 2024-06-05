use std::{collections::HashMap, num::NonZeroU32, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

impl Symbol {
    pub fn as_usize(self) -> usize {
        self.0.get() as usize
    }

    pub fn from_usize(value: usize) -> Option<Self> {
        NonZeroU32::new(value as u32).map(Symbol)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringInterner {
    pool: HashMap<Rc<str>, Symbol>,
    reverse_pool: Vec<Rc<str>>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            pool: HashMap::new(),
            reverse_pool: vec![Rc::from("")],
        }
    }

    pub fn find(&self, name: &str) -> Option<Symbol> {
        self.pool.get(name).copied()
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.reverse_pool
            .get(symbol.0.get() as usize)
            .map(|rc_str| &**rc_str)
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&symbol) = self.pool.get(name) {
            return symbol;
        }

        let symbol_id = NonZeroU32::new(self.reverse_pool.len() as u32)
            .expect("the length of reverse pool should not be 0 or exceed u32::MAX");

        let rc_name: Rc<str> = Rc::from(name);
        self.pool.insert(rc_name.clone(), Symbol(symbol_id));
        self.reverse_pool.push(rc_name);
        Symbol(symbol_id)
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}
