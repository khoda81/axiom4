use std::{
    collections::{hash_map::Entry, HashMap},
    num::{NonZeroU32, NonZeroU64},
};
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Node {
    Leaf(usize),
    BinaryOperator(usize),
    Reference(NodeId),
}

impl Node {
    fn value(self) -> usize {
        match self {
            Node::Leaf(value) | Node::BinaryOperator(value) => value,
            Node::Reference(NodeId(index)) => index,
        }
    }
}

/// Stores a node in a single u64
///
/// Stores the node kind in the last two bits.
/// | Last 2 Bits | Node Kind | Supported Range |
/// |-------------|-----------|-----------------|
/// | 01          | Reference | 0..2**62        |
/// | 10          | Leaf      | 0..2**62        |
/// | 11          | Operator  | 0..2**62        |
///
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Node64(NonZeroU64);

#[derive(Copy, Clone, Error, Debug)]
#[error("Node value {0} exceeds the supported range for Node64 (0..2**62)")]
pub struct OutOfBoundNode64Error(usize);

impl Node64 {
    const VALUE_MASK: u64 = u64::MAX >> 2;
    const VALUE_OFFSET: u64 = Self::VALUE_MASK + 1;

    #[allow(clippy::identity_op)]
    const REFERENCE_KIND: u64 = Self::VALUE_OFFSET * 0b01;
    const LEAF_KIND: u64 = Self::VALUE_OFFSET * 0b10;
    const OPERATOR_KIND: u64 = Self::VALUE_OFFSET * 0b11;

    pub fn new(node: Node) -> Result<Self, OutOfBoundNode64Error> {
        let value = node.value();
        if value > u64::MAX as usize {
            return Err(OutOfBoundNode64Error(value));
        }

        let value_u64 = value as u64;
        if (value_u64 & !Self::VALUE_MASK) != 0 {
            return Err(OutOfBoundNode64Error(value));
        }

        let kind = match node {
            Node::Leaf(_) => Self::LEAF_KIND,
            Node::Reference(_) => Self::REFERENCE_KIND,
            Node::BinaryOperator(_) => Self::OPERATOR_KIND,
        };

        let mask = kind | value_u64;
        let mask = NonZeroU64::new(mask).expect("kind mask should be nonzero");
        Ok(Self(mask))
    }

    pub fn into_node(self) -> Node {
        let value = (self.0.get() & Self::VALUE_MASK) as usize;
        let kind = self.0.get() & !Self::VALUE_MASK;

        if kind == Self::REFERENCE_KIND {
            Node::Reference(NodeId(value))
        } else if kind == Self::LEAF_KIND {
            Node::Leaf(value)
        } else {
            Node::BinaryOperator(value)
        }
    }
}

impl From<Node64> for Node {
    fn from(node64: Node64) -> Self {
        node64.into_node()
    }
}

impl TryFrom<Node> for Node64 {
    type Error = OutOfBoundNode64Error;

    fn try_from(node: Node) -> Result<Self, Self::Error> {
        Node64::new(node)
    }
}

/// Stores a node in a single u32
///
/// Stores the node kind in the last two bits.
/// | Last 2 Bits | Node Kind | Supported Range |
/// |-------------|-----------|-----------------|
/// | 01          | Reference | 0..2**30        |
/// | 10          | Leaf      | 0..2**30        |
/// | 11          | Operator  | 0..2**30        |
///
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Node32(NonZeroU32);

#[derive(Error, Debug, Copy, Clone)]
#[error("Node value {0} exceeds the supported range for Node64 (0..2**30)")]
pub struct OutOfBoundNode32Error(pub usize);

impl Node32 {
    const VALUE_MASK: u32 = u32::MAX >> 2;
    const VALUE_OFFSET: u32 = Self::VALUE_MASK + 1;

    #[allow(clippy::identity_op)]
    const REFERENCE_KIND: u32 = Self::VALUE_OFFSET * 0b01;
    const LEAF_KIND: u32 = Self::VALUE_OFFSET * 0b10;
    const OPERATOR_KIND: u32 = Self::VALUE_OFFSET * 0b11;

    pub fn new(node: Node) -> Result<Self, OutOfBoundNode32Error> {
        let value = node.value();
        if value > u32::MAX as usize {
            return Err(OutOfBoundNode32Error(value));
        }

        let value_u32 = value as u32;
        if (value_u32 & !Self::VALUE_MASK) != 0 {
            return Err(OutOfBoundNode32Error(value));
        }

        let kind = match node {
            Node::Leaf(_) => Self::LEAF_KIND,
            Node::Reference(_) => Self::REFERENCE_KIND,
            Node::BinaryOperator(_) => Self::OPERATOR_KIND,
        };

        let mask = kind | value_u32;
        let mask = NonZeroU32::new(mask).expect("kind mask should be nonzero");
        Ok(Self(mask))
    }

    pub fn into_node(self) -> Node {
        let value = (self.0.get() & Self::VALUE_MASK) as usize;
        let kind = self.0.get() & !Self::VALUE_MASK;

        if kind == Self::REFERENCE_KIND {
            Node::Reference(NodeId(value))
        } else if kind == Self::LEAF_KIND {
            Node::Leaf(value)
        } else {
            Node::BinaryOperator(value)
        }
    }
}

impl From<Node32> for Node {
    fn from(node32: Node32) -> Self {
        node32.into_node()
    }
}

impl TryFrom<Node> for Node32 {
    type Error = OutOfBoundNode32Error;

    fn try_from(node: Node) -> Result<Self, Self::Error> {
        Node32::new(node)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NodeId(usize);

#[derive(Copy, Clone, Debug, Error)]
#[error("Invalid Node ID: {0:?}")]
struct InvalidNodeId(NodeId);

/// An interner for a binary forest.
///
/// Assigns a uniquely identifiable id to every node in the forest.
/// Invariants:
/// - A Reference Node never points to another Reference, it is always resolved.
/// - The last node in the node array is always a non-reference.    
#[derive(Clone, Debug, Default)]
pub struct TreeInterner {
    nodes: Vec<Node64>,
    leaf_to_node: HashMap<usize, usize>,
    tree_to_node: HashMap<(usize, NodeId, NodeId), usize>,
}

impl TreeInterner {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            leaf_to_node: HashMap::new(),
            tree_to_node: HashMap::new(),
        }
    }

    pub fn intern_leaf(&mut self, leaf: usize) -> Result<NodeId, OutOfBoundNode64Error> {
        let node_id = match self.leaf_to_node.entry(leaf) {
            Entry::Occupied(occupied) => NodeId(*occupied.get()),
            Entry::Vacant(vacant) => {
                let node_id = self.nodes.len();
                let node = Node64::new(Node::Leaf(leaf))?;
                self.nodes.push(node);
                NodeId(*vacant.insert(node_id))
            }
        };

        Ok(node_id)
    }

    pub fn intern_operator(
        &mut self,
        operator: usize,
        left: NodeId,
        right: NodeId,
    ) -> Result<NodeId, OutOfBoundNode64Error> {
        let entry = match self.tree_to_node.entry((operator, left, right)) {
            Entry::Occupied(entry) => return Ok(NodeId(*entry.get())),
            Entry::Vacant(entry) => entry,
        };

        let left_node = self
            .nodes
            .get(left.0)
            .ok_or(InvalidNodeId(left))
            .expect("failed to resolve node ref left (node id is out of bound)");

        if matches!(left_node.into_node(), Node::Reference(_)) {
            panic!("failed to resolve node ref left (node id points to another reference)");
        }

        let right_node = self
            .nodes
            .get(right.0)
            .ok_or(InvalidNodeId(right))
            .expect("failed to resolve node ref right (node id is out of bound)");

        if matches!(right_node.into_node(), Node::Reference(_)) {
            panic!("failed to resolve node ref right (node id points to another reference)");
        }

        let last_node = self.nodes.len() - 1;
        let left_ref = Node64::new(Node::Reference(left))?;
        let right_ref = Node64::new(Node::Reference(right))?;
        let node = Node64::new(Node::BinaryOperator(operator))?;

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
        self.nodes[node_id.0].into_node()
    }

    pub fn right_child(&self, node_id: NodeId) -> NodeId {
        self.resolve_index(node_id.0 - 1)
    }

    pub fn left_child(&self, node_id: NodeId) -> NodeId {
        self.resolve_index(node_id.0 - 2)
    }

    fn resolve_index(&self, node_index: usize) -> NodeId {
        match self.nodes[node_index].into_node() {
            Node::Reference(node_id) => node_id,
            Node::Leaf(_) | Node::BinaryOperator(_) => NodeId(node_index),
        }
    }
}
