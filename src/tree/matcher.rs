use super::{interner::TreeInterner, NodeId};
use crate::tree::interner::InternalNode;
use binding::Var;
use std::collections::BTreeMap;
use thiserror::Error;

pub mod binding;

#[derive(Clone, Debug)]
pub struct Matcher<'a> {
    context: &'a TreeInterner,
    bindings: binding::Buckets,
    assignments: BTreeMap<Var, NodeId>,
}

impl<'a> Matcher<'a> {
    pub fn new(context: &'a TreeInterner) -> Self {
        Self {
            context,
            bindings: binding::Buckets::new(),
            assignments: BTreeMap::new(),
        }
    }

    pub fn r#match(mut self, a: NodeId, b: NodeId) -> Option<Self> {
        if a == b {
            return Some(self);
        }

        let node_a = self.context.resolve_internal(a);
        let node_b = self.context.resolve_internal(b);

        use InternalNode::{BinaryOperator as Bop, Term as Trm, Variable as Var};
        match (node_a, node_b) {
            (Var { id: a }, Var { id: b }) => self.bind(a, b),
            (Var { id }, _) => self.assign(id, b),
            (_, Var { id }) => self.assign(id, a),

            (Trm(symbol_a), Trm(symbol_b)) => (symbol_a == symbol_b).then_some(self),
            (Bop(symbol_a), Bop(symbol_b)) if symbol_a == symbol_b => {
                let a_left = self.context.left_child(a);
                let b_left = self.context.left_child(b);

                self = self.r#match(a_left, b_left)?;

                let a_right = self.context.right_child(a);
                let b_right = self.context.right_child(b);
                self.r#match(a_right, b_right)
            }

            _ => None,
        }
    }

    pub fn assign(mut self, variable: Var, value: NodeId) -> Option<Self> {
        let variable = self.bindings.find_root(variable);

        use std::collections::btree_map::Entry;
        match self.assignments.entry(variable) {
            Entry::Vacant(entry) => {
                entry.insert(value);
            }

            Entry::Occupied(entry) => {
                let prev_value = *entry.get();
                self = self.r#match(prev_value, value)?;
                self.assignments.insert(variable, value);
            }
        }

        Some(self)
    }

    pub fn bind(mut self, var_a: Var, var_b: Var) -> Option<Self> {
        if let (Some(old_root), new_root) = self.bindings.merge(var_a, var_b) {
            if let Some(old_value) = self.assignments.remove(&old_root) {
                if let Some(root_value) = self.assignments.get(&new_root).copied() {
                    return self.r#match(old_value, root_value);
                }
            }
        }

        Some(self)
    }

    pub fn bindings(&self) -> &binding::Buckets {
        &self.bindings
    }

    pub fn assignments(&self) -> &BTreeMap<Var, NodeId> {
        &self.assignments
    }

    pub fn finish(self) -> Match {
        Match {
            buckets: self.bindings,
            assignments: self.assignments,
            instance_cache: BTreeMap::new(),
        }
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Error)]
#[error("instance contains a recursive assignment")]
pub struct RecursiveInstance;

#[derive(Clone, Debug)]
pub struct Match {
    buckets: binding::Buckets,
    assignments: BTreeMap<Var, NodeId>,
    instance_cache: BTreeMap<NodeId, Result<NodeId, RecursiveInstance>>,
}

impl Match {
    pub fn instantiate(
        &mut self,
        tree: NodeId,
        context: &mut TreeInterner,
    ) -> Result<NodeId, RecursiveInstance> {
        use std::collections::btree_map::Entry;

        match self.instance_cache.entry(tree) {
            Entry::Occupied(entry) => return *entry.get(),
            // Mark this tree as recursive
            Entry::Vacant(entry) => entry.insert(Err(RecursiveInstance)),
        };

        let node = context.resolve_internal(tree);
        let instance = match node {
            InternalNode::Term(_) => tree,

            InternalNode::Variable { id } => {
                let root_var = self.buckets.find_root(id);

                match self.assignments.get(&root_var) {
                    Some(assignment) => self.instantiate(*assignment, context)?,
                    None => {
                        // This is a free variable, create a new variable with the same name
                        // BUG: This can cause variable collision since we may bind two variables
                        // with different buckets into the same name
                        let symbol = context.resolve_variable_symbol(id);
                        context.intern_variable(symbol).unwrap()
                    }
                }
            }

            InternalNode::BinaryOperator(symbol) => {
                let left = context.left_child(tree);
                let left = self.instantiate(left, context)?;

                let right = context.right_child(tree);
                let right = self.instantiate(right, context)?;

                context.intern_operator(symbol, left, right).unwrap()
            }
        };

        self.instance_cache.insert(tree, Ok(instance));
        Ok(instance)
    }
}
