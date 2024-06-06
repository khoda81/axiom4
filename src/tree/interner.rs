use super::{Node, NodeId, NodeKind};
use crate::lexer::interner::Symbol;
use std::{
    collections::{hash_map::Entry, HashMap},
    num::NonZeroU64,
};
use thiserror::Error;

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

    pub fn new(node: Node) -> Result<Self, Node64Error> {
        let symbol_mask = Self::symbol_mask(node)?;

        let mask = match node.kind {
            NodeKind::BinaryOperator => Self::OPERATOR_MASK | symbol_mask,
            NodeKind::Variable => Self::LEAF_MASK | ((symbol_mask << 1) + 1),
            NodeKind::Term => Self::LEAF_MASK | (symbol_mask << 1),
        };

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

    pub fn into_node(self) -> Result<Node, NodeId> {
        let value = (self.0.get() & Self::VALUE_MASK) as usize;
        let kind = self.0.get() & !Self::VALUE_MASK;

        match kind {
            Self::REFERENCE_MASK => Err(NodeId(value)),
            Self::LEAF_MASK => {
                let kind = match value & 1 {
                    0 => NodeKind::Term,
                    _ => NodeKind::Variable,
                };

                let symbol = Symbol::from_usize(value >> 1).unwrap();

                Ok(Node { kind, symbol })
            }

            Self::OPERATOR_MASK => Ok(Node {
                kind: NodeKind::BinaryOperator,
                symbol: Symbol::from_usize(value).unwrap(),
            }),

            unexpected_mask => {
                panic!("unexpected kind_mask={unexpected_mask:x} when converting Node64 to Node")
            }
        }
    }

    fn symbol_mask(node: Node) -> Result<u64, Node64Error> {
        let symbol_mask = node.symbol.as_u64();

        match node.kind {
            NodeKind::BinaryOperator => {
                if symbol_mask >= Self::VALUE_OFFSET {
                    return Err(Node64Error::OutOfBoundOperatorSymbol(node.symbol));
                }
            }

            NodeKind::Term | NodeKind::Variable => {
                if symbol_mask >= Self::VALUE_OFFSET >> 1 {
                    return Err(Node64Error::OutOfBoundLeafSymbol(node.symbol));
                }
            }
        }

        Ok(symbol_mask)
    }
}

impl TryFrom<Node> for Node64 {
    type Error = Node64Error;

    fn try_from(node: Node) -> Result<Self, Self::Error> {
        Node64::new(node)
    }
}

impl TryFrom<Node64> for Node {
    type Error = NodeId;

    fn try_from(node64: Node64) -> Result<Self, Self::Error> {
        node64.into_node()
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
    variables: HashMap<Symbol, usize>,
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
            terms: HashMap::new(),
            operators: HashMap::new(),
        }
    }

    pub fn intern_term(&mut self, term_symbol: Symbol) -> Result<NodeId, Node64Error> {
        let node = Node::new_term(term_symbol).try_into()?;
        let node_id = self.terms.entry(term_symbol).or_insert_with(|| {
            self.nodes.push(node);
            self.nodes.len() - 1
        });

        Ok(NodeId(*node_id))
    }

    pub fn intern_variable(&mut self, var_symbol: Symbol) -> Result<NodeId, Node64Error> {
        let node = Node::new_variable(var_symbol).try_into()?;
        let node_id = self.variables.entry(var_symbol).or_insert_with(|| {
            self.nodes.push(node);
            self.nodes.len() - 1
        });

        Ok(NodeId(*node_id))
    }

    pub fn intern_operator(
        &mut self,
        operator: Symbol,
        left: NodeId,
        right: NodeId,
    ) -> Result<NodeId, Node64Error> {
        let entry = match self.operators.entry((operator, left, right)) {
            Entry::Occupied(entry) => return Ok(NodeId(*entry.get())),
            Entry::Vacant(entry) => entry,
        };

        self.nodes[left.0]
            .into_node()
            .expect("left node id points to a reference node");

        self.nodes[right.0]
            .into_node()
            .expect("right node id points to a reference node");

        let last_node = self.nodes.len() - 1;
        let left_ref = Node64::new_reference(left)?;
        let right_ref = Node64::new_reference(right)?;
        let node = Node::new_binary_operator(operator).try_into()?;

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

    pub fn resolve(&self, node_id: NodeId) -> Node {
        self.nodes[node_id.0]
            .into_node()
            .expect("given node id points to a reference node")
    }

    pub fn right_child(&self, node_id: NodeId) -> NodeId {
        self.resolve_index(node_id.0 - 1)
    }

    pub fn left_child(&self, node_id: NodeId) -> NodeId {
        self.resolve_index(node_id.0 - 2)
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item = Result<Node, usize>> + '_ {
        self.nodes
            .iter()
            .copied()
            .map(|node64| match node64.into_node() {
                Ok(node) => Ok(node),
                Err(NodeId(index)) => Err(index),
            })
    }

    fn resolve_index(&self, node_index: usize) -> NodeId {
        match self.nodes[node_index].into_node() {
            Ok(_) => NodeId(node_index),
            Err(node_id) => node_id,
        }
    }
}
