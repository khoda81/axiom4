use std::collections::BTreeMap;

use thiserror::Error;

use super::{NodeId, NodeKind, TreeInterner};
use crate::lexer::interner::Symbol;

pub mod binding;

#[derive(Clone, Debug)]
pub struct Matcher<'a> {
    context: &'a TreeInterner,
    bindings: binding::Bindings,
    assignments: BTreeMap<Symbol, NodeId>,
}

impl<'a> Matcher<'a> {
    pub fn new(context: &'a TreeInterner) -> Self {
        Self {
            context,
            bindings: binding::Bindings::new(),
            assignments: BTreeMap::new(),
        }
    }

    pub fn r#match(mut self, a: NodeId, b: NodeId) -> Option<Self> {
        if a == b {
            return Some(self);
        }

        let node_a = self.context.resolve(a);
        let node_b = self.context.resolve(b);

        use NodeKind::{BinaryOperator as Bop, Term as Trm, Variable as Var};
        match (node_a.kind, node_b.kind) {
            (Var, Var) => self.bind(node_a.symbol, node_b.symbol),
            (Var, _) => self.assign(node_a.symbol, b),
            (_, Var) => self.assign(node_b.symbol, a),

            (Trm, Trm) => (node_a.symbol == node_b.symbol).then_some(self),
            (Bop, Bop) if node_a.symbol == node_b.symbol => {
                let a_left = self.context.left_child(a);
                let b_left = self.context.left_child(b);

                self = self.r#match(a_left, b_left)?;

                let a_right = self.context.right_child(a);
                let b_right = self.context.right_child(b);
                self.r#match(a_right, b_right)
            }

            (Trm, Bop) | (Bop, Trm) | (Bop, Bop) => None,
        }
    }

    pub fn assign(mut self, variable: Symbol, value: NodeId) -> Option<Self> {
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

    pub fn bind(mut self, var_a: Symbol, var_b: Symbol) -> Option<Self> {
        if let (Some(old_root), new_root) = self.bindings.bind(var_a, var_b) {
            if let Some(old_value) = self.assignments.remove(&old_root) {
                if let Some(root_value) = self.assignments.get(&new_root).copied() {
                    return self.r#match(old_value, root_value);
                }
            }
        }

        Some(self)
    }

    pub fn bindings(&self) -> &binding::Bindings {
        &self.bindings
    }

    pub fn assignments(&self) -> &BTreeMap<Symbol, NodeId> {
        &self.assignments
    }

    pub fn finish(self) -> Match {
        Match {
            bindings: self.bindings,
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
    bindings: binding::Bindings,
    assignments: BTreeMap<Symbol, NodeId>,
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

        let node = context.resolve(tree);
        let instance = match node.kind {
            NodeKind::Term => tree,

            NodeKind::Variable => {
                let root_var = self.bindings.find_root(node.symbol);
                match self.assignments.get(&root_var) {
                    Some(assignment) => self.instantiate(*assignment, context)?,
                    // This is a free variable
                    None => context.intern_variable(root_var).unwrap(),
                }
            }

            NodeKind::BinaryOperator => {
                let left = context.left_child(tree);
                let left = self.instantiate(left, context)?;

                let right = context.right_child(tree);
                let right = self.instantiate(right, context)?;

                context.intern_operator(node.symbol, left, right).unwrap()
            }
        };

        self.instance_cache.insert(tree, Ok(instance));
        Ok(instance)
    }
}
