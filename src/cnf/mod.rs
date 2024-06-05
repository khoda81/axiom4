use std::collections::VecDeque;

use crate::tree::NodeId;
use section_vec::{Section, SectionVec};

pub mod section_vec;

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
pub struct ConjunctionVec {
    clauses: VecDeque<NodeId>,
    num_positives: usize,
}

impl ConjunctionVec {
    pub fn new() -> Self {
        Self {
            clauses: VecDeque::new(),
            num_positives: 0,
        }
    }

    pub fn push(&mut self, sign: Sign, clause: NodeId) {
        match sign {
            Sign::Positive => {
                self.num_positives += 1;
                self.clauses.push_front(clause)
            }
            Sign::Negative => self.clauses.push_back(clause),
        }
    }

    pub fn drain_positive(&mut self) -> impl Iterator<Item = NodeId> + '_ {
        let num_positives = self.num_positives;
        self.num_positives = 0;
        self.clauses.drain(..num_positives)
    }

    pub fn drain_negatives(&mut self) -> impl Iterator<Item = NodeId> + '_ {
        self.clauses.drain(self.num_positives..)
    }

    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }

    pub fn len(&self) -> usize {
        self.clauses.len()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ConjunctionRef<'a> {
    pub positives: &'a [NodeId],
    pub negatives: &'a [NodeId],
}

impl ConjunctionRef<'_> {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ConjunctionId(Section);

#[derive(Clone, Debug)]
pub struct CNF {
    positive_clauses: SectionVec<NodeId>,
    negative_clauses: SectionVec<NodeId>,
}

impl CNF {
    pub fn new() -> Self {
        Self {
            positive_clauses: SectionVec::new(),
            negative_clauses: SectionVec::new(),
        }
    }

    pub fn push(&mut self, mut conjunction: ConjunctionVec) {
        let pos_section = self.positive_clauses.new_section();
        let neg_section = self.negative_clauses.new_section();

        debug_assert_eq!(
            pos_section, neg_section,
            "expected positive and negative section count to be equal"
        );

        self.positive_clauses.extend(conjunction.drain_positive());
        self.negative_clauses.extend(conjunction.drain_negatives());
    }

    pub fn find_conjunction(&self, index: usize, sign: Sign) -> ConjunctionId {
        let section = match sign {
            Sign::Positive => self.positive_clauses.find_section(index),
            Sign::Negative => self.negative_clauses.find_section(index),
        };

        ConjunctionId(section)
    }

    pub fn conjunction_ref(&self, ConjunctionId(section): ConjunctionId) -> ConjunctionRef {
        let positives = self
            .positive_clauses
            .section(section)
            .expect("invalid section");

        let negatives = self
            .negative_clauses
            .section(section)
            .expect("invalid section");

        ConjunctionRef {
            positives,
            negatives,
        }
    }
}

impl Default for CNF {
    fn default() -> Self {
        Self::new()
    }
}
