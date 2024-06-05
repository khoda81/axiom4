use std::{
    collections::{hash_map::Entry, HashMap},
    num::NonZeroU64,
};
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Node {
    Leaf(usize),
    BinaryOperator(usize),
}

impl Node {
    pub fn value(self) -> usize {
        match self {
            Node::Leaf(value) | Node::BinaryOperator(value) => value,
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
        let kind = match node {
            Node::Leaf(_) => Self::LEAF_KIND,
            Node::BinaryOperator(_) => Self::OPERATOR_KIND,
        };

        let mask = kind | Self::value_mask(value)?;
        let mask = NonZeroU64::new(mask).expect("kind mask should be nonzero");
        Ok(Self(mask))
    }

    pub fn new_reference(node_id: NodeId) -> Result<Self, OutOfBoundNode64Error> {
        let value = node_id.0;
        let mask = Self::REFERENCE_KIND | Self::value_mask(value)?;
        let mask = NonZeroU64::new(mask).expect("kind mask should be nonzero");
        Ok(Self(mask))
    }

    pub fn into_node(self) -> Result<Node, NodeId> {
        let value = (self.0.get() & Self::VALUE_MASK) as usize;
        let kind = self.0.get() & !Self::VALUE_MASK;

        if kind == Self::REFERENCE_KIND {
            Err(NodeId(value))
        } else if kind == Self::LEAF_KIND {
            Ok(Node::Leaf(value))
        } else {
            Ok(Node::BinaryOperator(value))
        }
    }

    fn value_mask(value: usize) -> Result<u64, OutOfBoundNode64Error> {
        if value > u64::MAX as usize {
            return Err(OutOfBoundNode64Error(value));
        }

        let value_u64 = value as u64;
        if (value_u64 & !Self::VALUE_MASK) != 0 {
            return Err(OutOfBoundNode64Error(value));
        }

        Ok(value_u64)
    }
}

impl TryFrom<Node> for Node64 {
    type Error = OutOfBoundNode64Error;

    fn try_from(node: Node) -> Result<Self, Self::Error> {
        Node64::new(node)
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
    // TODO: Try BtreeMap
    leaf_to_node: HashMap<usize, usize>,
    // TODO: Try BtreeMap
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

        if left_node.into_node().is_err() {
            panic!("failed to resolve node ref left (node id points to another reference)");
        }

        let right_node = self
            .nodes
            .get(right.0)
            .ok_or(InvalidNodeId(right))
            .expect("failed to resolve node ref right (node id is out of bound)");

        if right_node.into_node().is_err() {
            panic!("failed to resolve node ref right (node id points to another reference)");
        }

        let last_node = self.nodes.len() - 1;
        let left_ref = Node64::new_reference(left)?;
        let right_ref = Node64::new_reference(right)?;
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

    pub fn into_nodes(self) -> Vec<Result<Node, usize>> {
        self.nodes
            .into_iter()
            .map(|node64| match Node64::into_node(node64) {
                Ok(node) => Ok(node),
                Err(NodeId(index)) => Err(index),
            })
            .collect()
    }

    fn resolve_index(&self, node_index: usize) -> NodeId {
        match self.nodes[node_index].into_node() {
            Ok(_) => NodeId(node_index),
            Err(node_id) => node_id,
        }
    }
}
