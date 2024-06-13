#[derive(Clone, Debug)]
pub struct Sections<T> {
    splits: Vec<T>,
}

pub enum Section<T> {
    First(std::ops::RangeTo<T>),
    Middle(std::ops::Range<T>),
    Last(std::ops::RangeFrom<T>),
}

impl<T> Sections<T> {
    pub fn new() -> Self {
        Sections { splits: Vec::new() }
    }

    pub fn splits(&self) -> &[T] {
        &self.splits
    }

    pub fn len(&self) -> usize {
        self.splits.len()
    }

    pub fn is_empty(&self) -> bool {
        self.splits.is_empty()
    }
}

impl<T: Ord> Sections<T> {
    pub fn split_last(&mut self, boundary: T) -> Result<(), T> {
        let last_split = self.splits.last();
        if last_split.is_some_and(|last_split| last_split > &boundary) {
            Err(boundary)
        } else {
            self.splits.push(boundary);
            Ok(())
        }
    }

    pub fn find_section_for(&self, item: &T) -> usize {
        self.splits
            .partition_point(|section_end| section_end <= item)
    }
}

impl<T: Clone> Sections<T> {
    pub fn first(&self) -> Option<std::ops::RangeTo<T>> {
        self.splits
            .first()
            .map(|first_split| ..(first_split.clone()))
    }

    pub fn last(&self) -> Option<std::ops::RangeFrom<T>> {
        self.splits.last().map(|last_split| (last_split.clone())..)
    }

    pub fn section(&self, section_idx: usize) -> Option<Section<T>> {
        let end = self.splits.get(section_idx).cloned();

        match section_idx.checked_sub(1) {
            None => Some(Section::First(..end?)),
            Some(previous) => {
                let start = self.splits.get(previous)?.clone();

                match end {
                    Some(end) => Some(Section::Middle(start..end)),
                    None => Some(Section::Last(start..)),
                }
            }
        }
    }

    pub fn iter_sections(&self) -> impl Iterator<Item = Section<T>> + '_ {
        (0..self.splits.len()).map_while(|idx| self.section(idx))
    }
}

impl<T> Default for Sections<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct SectionVec<T> {
    items: Vec<T>,
    sections: Sections<usize>,
}

impl<T> SectionVec<T> {
    pub fn new() -> Self {
        SectionVec {
            items: Vec::new(),
            sections: Sections::new(),
        }
    }

    pub fn push_section(&mut self) {
        self.sections
            .split_last(self.items.len())
            .expect("the new split should be greater/equal to previous split");
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item)
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) {
        self.items.extend(iter)
    }

    pub fn section_index(&self, item_index: usize) -> usize {
        self.sections.find_section_for(&item_index)
    }

    pub fn section_range(&self, section_idx: usize) -> Option<std::ops::Range<usize>> {
        match self.sections.section(section_idx)? {
            Section::First(range) => Some(0..range.end),
            Section::Middle(range) => Some(range),
            Section::Last(range) => Some(range.start..self.items.len()),
        }
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
        (0..self.sections.len()).map_while(|idx| self.section_slice(idx))
    }

    pub fn section_indices(&self) -> &[usize] {
        self.sections.splits()
    }

    pub fn current_section_index(&self) -> usize {
        self.sections.len()
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
