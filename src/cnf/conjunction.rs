use std::{collections::VecDeque, fmt::Display};

use crate::{
    lexer::interner::StringInterner,
    tree::{interner::TreeInterner, NodeId},
};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum Sign {
    #[default]
    Positive,
    Negative,
}

impl std::ops::Not for Sign {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Sign::Positive => Sign::Negative,
            Sign::Negative => Sign::Positive,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Conjunction {
    clauses: VecDeque<NodeId>,
    num_positives: usize,
}

impl Conjunction {
    pub fn new() -> Self {
        Self {
            clauses: VecDeque::new(),
            num_positives: 0,
        }
    }

    pub fn push(&mut self, sign: Sign, clause: NodeId) {
        match sign {
            Sign::Negative => self.clauses.push_back(clause),
            Sign::Positive => {
                self.num_positives += 1;
                self.clauses.push_front(clause)
            }
        }
    }

    pub fn pop(&mut self, sign: Sign) -> Option<NodeId> {
        match sign {
            Sign::Negative => self.clauses.pop_back(),
            Sign::Positive => {
                self.num_positives -= 1;
                self.clauses.pop_front()
            }
        }
    }

    pub fn drain(&mut self, sign: Sign) -> impl Iterator<Item = NodeId> + '_ {
        match sign {
            Sign::Negative => self.clauses.drain(self.num_positives..),
            Sign::Positive => {
                let num_positives = self.num_positives;
                self.num_positives = 0;
                self.clauses.drain(..num_positives)
            }
        }
    }

    pub fn iter(&self, sign: Sign) -> impl Iterator<Item = &NodeId> + '_ {
        match sign {
            Sign::Negative => self.clauses.range(self.num_positives..),
            Sign::Positive => self.clauses.range(..self.num_positives),
        }
    }

    pub fn as_conjunction_ref(&mut self) -> ConjunctionRef<'_> {
        let clauses = self.clauses.make_contiguous();
        let (positives, negatives) = clauses.split_at(self.num_positives);
        ConjunctionRef {
            positives,
            negatives,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }

    pub fn len(&self) -> usize {
        self.clauses.len()
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct ConjunctionRef<'a> {
    pub positives: &'a [NodeId],
    pub negatives: &'a [NodeId],
}

impl<'a> ConjunctionRef<'a> {
    pub fn format(
        &'_ self,
        tree_interner: &'a TreeInterner,
        string_interner: &'a StringInterner,
    ) -> ConjunctionFormatter<'a> {
        ConjunctionFormatter {
            conjunction: *self,
            tree_interner,
            string_interner,
            print_scopes: false,
            positive_first: false,
        }
    }
}

#[must_use]
pub struct ConjunctionFormatter<'a> {
    pub tree_interner: &'a TreeInterner,
    pub string_interner: &'a StringInterner,
    pub print_scopes: bool,
    pub positive_first: bool,
    pub conjunction: ConjunctionRef<'a>,
}

impl Display for ConjunctionFormatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.positive_first {
            self.format_positives(f)?;
            self.format_negatives(f)?;
        } else {
            self.format_negatives(f)?;
            self.format_positives(f)?;
        }

        Ok(())
    }
}

impl<'a> ConjunctionFormatter<'a> {
    fn format_clause(&self, clause: NodeId) -> crate::tree::interner::TreeFormatter<'_> {
        crate::tree::interner::TreeFormatter {
            node_id: clause,
            tree_interner: self.tree_interner,
            string_interner: self.string_interner,
            print_scopes: self.print_scopes,
            parent_precedence: 12,
        }
    }

    fn format_positives(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.conjunction.positives.iter().try_for_each(|&clause| {
            write!(f, " | {formatter}", formatter = self.format_clause(clause))
        })
    }

    fn format_negatives(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.conjunction.negatives.iter().try_for_each(|&clause| {
            write!(f, " ! {formatter}", formatter = self.format_clause(clause))
        })
    }
}
