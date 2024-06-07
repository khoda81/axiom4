use crate::tree::NodeId;
pub use conjunction::{Conjunction, ConjunctionRef, Sign};
use section_vec::SectionVec;

mod conjunction;
pub mod section_vec;

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

    pub fn assert(&mut self, conjunction: Conjunction) {
        self.positive_clauses.push_section();
        self.negative_clauses.push_section();

        debug_assert_eq!(
            self.positive_clauses.section_indices().len(),
            self.negative_clauses.section_indices().len(),
            "expected positive and negative section count to be equal"
        );

        self.positive_clauses
            .extend(conjunction.iter(Sign::Positive).copied());
        self.negative_clauses
            .extend(conjunction.iter(Sign::Negative).copied());
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
            .skip(1)
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
