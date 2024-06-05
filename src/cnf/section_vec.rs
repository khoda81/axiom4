#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Section(usize);

#[derive(Clone, Debug)]
pub struct SectionVec<T> {
    items: Vec<T>,
    sections: Vec<usize>,
}

impl<T> SectionVec<T> {
    pub fn new() -> Self {
        SectionVec {
            items: Vec::new(),
            sections: Vec::new(),
        }
    }

    pub fn new_section(&mut self) -> Section {
        let item_id = self.items.len();
        self.sections.push(item_id);
        Section(self.items.len() - 1)
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item)
    }

    pub fn find_section(&mut self, index: usize) -> Section {
        match self.sections.binary_search(&index) {
            Ok(index) => Section(index),
            Err(index) => Section(index - 1),
        }
    }

    pub fn section(&self, Section(section_idx): Section) -> Option<&[T]> {
        let &start = self.sections.get(section_idx)?;
        let &end = self
            .sections
            .get(section_idx + 1)
            .unwrap_or(&self.items.len());

        Some(&self.items[start..end])
    }
}

impl<T> Default for SectionVec<T> {
    fn default() -> Self {
        Self::new()
    }
}
