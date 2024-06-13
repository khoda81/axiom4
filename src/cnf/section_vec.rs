#[derive(Clone, Debug)]
pub struct SectionVec<T> {
    items: Vec<T>,
    section_indices: Vec<usize>,
}

impl<T> SectionVec<T> {
    pub fn new() -> Self {
        SectionVec {
            items: Vec::new(),
            section_indices: Vec::new(),
        }
    }

    pub fn push_section(&mut self) {
        self.section_indices.push(self.items.len());
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item)
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) {
        self.items.extend(iter)
    }

    pub fn section_index(&self, item_index: usize) -> usize {
        self.section_indices
            .partition_point(|section_end| *section_end <= item_index)
    }

    pub fn section_range(&self, section_idx: usize) -> Option<std::ops::Range<usize>> {
        let start = match section_idx.checked_sub(1) {
            None => 0,
            Some(previous) => *self.section_indices.get(previous)?,
        };

        let end = self.section_indices.get(section_idx).copied();
        Some(start..end.unwrap_or(self.items.len()))
    }

    pub fn section_slice(&self, section_idx: usize) -> Option<&[T]> {
        let range = self.section_range(section_idx)?;
        Some(&self.items[range])
    }

    pub fn section_mut_slice(&mut self, section_idx: usize) -> Option<&mut [T]> {
        let range = self.section_range(section_idx)?;
        Some(&mut self.items[range])
    }

    pub fn iter_sections(&self) -> impl Iterator<Item = &[T]> {
        (0..self.section_indices.len()).map_while(|idx| self.section_slice(idx))
    }

    pub fn section_indices(&self) -> &[usize] {
        &self.section_indices
    }

    pub fn current_section(&self) -> usize {
        self.section_indices.len()
    }

    pub fn items(&self) -> &[T] {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut [T] {
        &mut self.items
    }
}

impl<T> Default for SectionVec<T> {
    fn default() -> Self {
        Self::new()
    }
}
