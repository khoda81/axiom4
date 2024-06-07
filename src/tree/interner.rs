use super::{Node, NodeId};
use crate::{cnf::section_vec::SectionVec, lexer::interner::Symbol};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Display,
    num::NonZeroU64,
};
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum InternalNode {
    BinaryOperator(Symbol),
    Term(Symbol),
    Variable { id: usize },
}

/// Stores a node in a single u64
///
/// Stores the node kind in the last two bits.
/// | Last 2 Bits | Node Kind | Supported Range |
/// |-------------|-----------|-----------------|
/// | 01xxx..xx   | Operator  | 0..2**62        |
/// | 10xxx..x0   | Term      | 0..2**61        |
/// | 10xxx..x1   | Variable  | 0..2**61        |
/// | 11xxx..xx   | Reference | 0..2**62        |
///
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Node64(NonZeroU64);

#[derive(Copy, Clone, Error, Debug)]
pub enum Node64Error {
    #[error("Variable exceeds the supported range for a leaf (0..2**61)")]
    OutOfBoundVariable(usize),

    #[error("Symbol exceeds the supported range for a leaf (0..2**61)")]
    OutOfBoundLeafSymbol(Symbol),

    #[error("Symbol exceeds the supported range for a binary operator (0..2**62)")]
    OutOfBoundOperatorSymbol(Symbol),

    #[error("NodeId exceeds the supported range for a reference (0..2**62)")]
    OutOfBoundNodeId(NodeId),
}

impl Node64 {
    const VALUE_MASK: u64 = u64::MAX >> 2;
    const VALUE_OFFSET: u64 = Self::VALUE_MASK + 1;

    #[allow(clippy::identity_op)]
    const OPERATOR_MASK: u64 = Self::VALUE_OFFSET * 0b01;
    const LEAF_MASK: u64 = Self::VALUE_OFFSET * 0b10;
    const REFERENCE_MASK: u64 = Self::VALUE_OFFSET * 0b11;

    pub fn new(node: InternalNode) -> Result<Self, Node64Error> {
        let mask = Self::node_mask(node)?;
        let mask = NonZeroU64::new(mask).expect("node64 mask should be nonzero");
        Ok(Self(mask))
    }

    pub fn new_reference(node_id: NodeId) -> Result<Self, Node64Error> {
        let node_mask: u64 = node_id
            .0
            .try_into()
            .map_err(|_| Node64Error::OutOfBoundNodeId(node_id))?;

        if node_mask >= Self::VALUE_OFFSET {
            return Err(Node64Error::OutOfBoundNodeId(node_id));
        }

        let mask = Self::REFERENCE_MASK | node_mask;
        let mask = NonZeroU64::new(mask).expect("node64 mask should be nonzero");
        Ok(Self(mask))
    }

    pub fn into_node(self) -> Result<InternalNode, NodeId> {
        let value = (self.0.get() & Self::VALUE_MASK) as usize;
        let kind = self.0.get() & !Self::VALUE_MASK;

        match kind {
            Self::REFERENCE_MASK => Err(NodeId(value)),
            Self::LEAF_MASK => match value & 1 {
                0 => Ok(InternalNode::Term(Symbol::from_usize(value >> 1).unwrap())),
                _ => Ok(InternalNode::Variable { id: value >> 1 }),
            },

            Self::OPERATOR_MASK => Ok(InternalNode::BinaryOperator(
                Symbol::from_usize(value).unwrap(),
            )),

            unexpected_mask => {
                panic!("unexpected kind_mask={unexpected_mask:x} when converting Node64 to Node")
            }
        }
    }

    fn node_mask(node: InternalNode) -> Result<u64, Node64Error> {
        match node {
            InternalNode::BinaryOperator(symbol) => {
                let symbol_mask = symbol.as_u64();
                (symbol_mask < Self::VALUE_OFFSET)
                    .then_some(Self::OPERATOR_MASK | symbol_mask)
                    .ok_or(Node64Error::OutOfBoundOperatorSymbol(symbol))
            }

            InternalNode::Term(symbol) => {
                let symbol_mask = symbol.as_u64();

                if symbol_mask < Self::VALUE_OFFSET >> 1 {
                    Ok(Self::LEAF_MASK | (symbol_mask << 1))
                } else {
                    Err(Node64Error::OutOfBoundLeafSymbol(symbol))
                }
            }

            InternalNode::Variable { id } => {
                let value_mask: u64 = id
                    .try_into()
                    .map_err(|_| Node64Error::OutOfBoundVariable(id))?;

                if value_mask < Self::VALUE_OFFSET >> 1 {
                    Ok(Self::LEAF_MASK | ((value_mask << 1) + 1))
                } else {
                    Err(Node64Error::OutOfBoundVariable(id))
                }
            }
        }
    }
}

impl TryFrom<InternalNode> for Node64 {
    type Error = Node64Error;

    fn try_from(node: InternalNode) -> Result<Self, Self::Error> {
        Node64::new(node)
    }
}

impl TryFrom<Node64> for InternalNode {
    type Error = NodeId;

    fn try_from(node64: Node64) -> Result<Self, Self::Error> {
        node64.into_node()
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl Display for ScopeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// An interner for a binary forest.
///
/// Assigns a uniquely identifiable id to every node in the forest.
/// Invariants:
/// - A Reference Node never points to another Reference, it is always resolved.
/// - The last node in the node array is always a non-reference.    
#[derive(Clone, Debug, Default)]
pub struct TreeInterner {
    // TODO: Try Storing kinds separately than values
    nodes: Vec<Node64>,
    // TODO: Try BtreeMap
    variables: HashMap<(Symbol, ScopeId), usize>,
    scopes: SectionVec<Symbol>,
    // TODO: Try BtreeMap
    terms: HashMap<Symbol, usize>,
    // TODO: Try BtreeMap
    operators: HashMap<(Symbol, NodeId, NodeId), usize>,
}

impl TreeInterner {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            variables: HashMap::new(),
            scopes: SectionVec::new(),
            terms: HashMap::new(),
            operators: HashMap::new(),
        }
    }

    pub fn intern_term(&mut self, term_symbol: Symbol) -> Result<NodeId, Node64Error> {
        let node = InternalNode::Term(term_symbol).try_into()?;
        let node_id = self.terms.entry(term_symbol).or_insert_with(|| {
            self.nodes.push(node);
            self.nodes.len() - 1
        });

        Ok(NodeId(*node_id))
    }

    pub fn intern_variable(&mut self, var_symbol: Symbol) -> Result<NodeId, Node64Error> {
        let scope = ScopeId(self.scopes.current_section() as u32);
        let id = self.scopes.items().len();

        let node = InternalNode::Variable { id }.try_into()?;
        let node_id = self
            .variables
            .entry((var_symbol, scope))
            .or_insert_with(|| {
                self.scopes.push(var_symbol);
                self.nodes.push(node);
                self.nodes.len() - 1
            });

        Ok(NodeId(*node_id))
    }

    pub fn intern_variable_with_scope(
        &mut self,
        var_symbol: Symbol,
        scope: ScopeId,
    ) -> Option<NodeId> {
        self.variables
            .get(&(var_symbol, scope))
            .copied()
            .map(NodeId)
    }

    pub fn push_scope(&mut self) {
        let last_scope = self.scopes.current_section();
        if let Some([]) = self.scopes.section_slice(last_scope) {
            // Last scope was empty skip
            return;
        }

        self.scopes.push_section()
    }

    pub fn variable_symbol(&self, var_index: usize) -> Option<Symbol> {
        self.scopes.items().get(var_index).copied()
    }

    pub fn intern_operator(
        &mut self,
        operator: Symbol,
        left: NodeId,
        right: NodeId,
    ) -> Result<NodeId, Node64Error> {
        // Check sanity of node_refs
        #[cfg(debug_assertions)]
        {
            self.resolve(left);
            self.resolve(right);
        }

        let entry = match self.operators.entry((operator, left, right)) {
            Entry::Occupied(entry) => return Ok(NodeId(*entry.get())),
            Entry::Vacant(entry) => entry,
        };

        let last_node = self.nodes.len() - 1;
        let left_ref = Node64::new_reference(left)?;
        let right_ref = Node64::new_reference(right)?;
        let node = InternalNode::BinaryOperator(operator).try_into()?;

        if left.0 == last_node - 1 && right.0 == last_node {
            // Don't push children, last two nodes are children
            self.nodes.extend([node]);
        } else if left.0 == last_node {
            // Only push right, left child is in place
            self.nodes.extend([right_ref, node]);
        } else {
            // Push both, non of the children on in place
            self.nodes.extend([left_ref, right_ref, node]);
        }

        let node_id = self.nodes.len() - 1;
        Ok(NodeId(*entry.insert(node_id)))
    }

    pub fn resolve_internal(&self, node_id: NodeId) -> InternalNode {
        self.nodes[node_id.0]
            .into_node()
            .expect("given node id points to a reference node")
    }

    pub fn resolve(&self, node_id: NodeId) -> Node {
        match self.resolve_internal(node_id) {
            InternalNode::BinaryOperator(symbol) => Node::new_binary_operator(symbol),
            InternalNode::Term(symbol) => Node::new_term(symbol),
            InternalNode::Variable { id } => {
                let symbol = self.resolve_variable_symbol(id);
                let scope = self.resolve_variable_scope(id);

                Node::new_variable(symbol, scope)
            }
        }
    }

    pub fn resolve_variable_scope(&self, id: usize) -> ScopeId {
        ScopeId(self.scopes.section_index(id).try_into().unwrap())
    }

    pub fn resolve_variable_symbol(&self, id: usize) -> Symbol {
        self.scopes.items()[id]
    }

    pub fn right_child(&self, node_id: NodeId) -> NodeId {
        self.resolve_index(node_id.0 - 1)
    }

    pub fn left_child(&self, node_id: NodeId) -> NodeId {
        self.resolve_index(node_id.0 - 2)
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item = Result<InternalNode, NodeId>> + '_ {
        self.nodes
            .iter()
            .copied()
            .map(|node64| match node64.into_node() {
                Ok(node) => Ok(node),
                Err(node_id) => Err(node_id),
            })
    }

    fn resolve_index(&self, node_index: usize) -> NodeId {
        match self.nodes[node_index].into_node() {
            Ok(_) => NodeId(node_index),
            Err(node_id) => node_id,
        }
    }
}
