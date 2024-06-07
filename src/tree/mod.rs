use crate::{lexer::interner::Symbol, tree::interner::ScopeId};
pub use interner::NodeId;

pub mod interner;
pub mod matcher;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NodeKind {
    Term,
    Variable { scope: ScopeId },
    BinaryOperator,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub symbol: Symbol,
}

impl Node {
    pub fn new(kind: NodeKind, symbol: Symbol) -> Self {
        Self { kind, symbol }
    }

    pub fn new_term(symbol: Symbol) -> Self {
        Self::new(NodeKind::Term, symbol)
    }

    pub fn new_variable(symbol: Symbol, scope: ScopeId) -> Self {
        Self::new(NodeKind::Variable { scope }, symbol)
    }

    pub fn new_binary_operator(symbol: Symbol) -> Self {
        Self::new(NodeKind::BinaryOperator, symbol)
    }
}
