use crate::tree::NodeId;
use section_vec::SectionVec;
use std::collections::VecDeque;

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
            Sign::Positive => {
                self.num_positives += 1;
                self.clauses.push_front(clause)
            }

            Sign::Negative => self.clauses.push_back(clause),
        }
    }

    pub fn drain_positives(&mut self) -> impl Iterator<Item = NodeId> + '_ {
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

    pub fn push(&mut self, mut conjunction: Conjunction) {
        self.positive_clauses.push_section();
        self.negative_clauses.push_section();

        debug_assert_eq!(
            self.positive_clauses.section_indices().len(),
            self.negative_clauses.section_indices().len(),
            "expected positive and negative section count to be equal"
        );

        self.positive_clauses.extend(conjunction.drain_positives());
        self.negative_clauses.extend(conjunction.drain_negatives());
    }

    pub fn find_conjunction(&self, clause_index: usize, sign: Sign) -> usize {
        match sign {
            Sign::Positive => self.positive_clauses.section_index(clause_index),
            Sign::Negative => self.negative_clauses.section_index(clause_index),
        }
    }

    pub fn conjunction_ref(&self, conjunction_index: usize) -> Option<ConjunctionRef> {
        Some(ConjunctionRef {
            positives: self.positive_clauses.section_slice(conjunction_index)?,
            negatives: self.negative_clauses.section_slice(conjunction_index)?,
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = ConjunctionRef> {
        let positives = self.positive_clauses.iter_sections();
        let negatives = self.negative_clauses.iter_sections();

        positives
            .zip(negatives)
            .map(|(positives, negatives)| ConjunctionRef {
                positives,
                negatives,
            })
    }

    pub fn positive_clauses(&self) -> &[NodeId] {
        self.positive_clauses.items()
    }

    pub fn negative_clauses(&self) -> &[NodeId] {
        self.negative_clauses.items()
    }
}

impl Default for CNF {
    fn default() -> Self {
        Self::new()
    }
}
