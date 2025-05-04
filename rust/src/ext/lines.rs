use std::str::Lines;

use crate::ext::traits::empty_items::EmptyItems;

impl EmptyItems for Lines<'_> {
    fn empty(self) -> impl Iterator<Item = Self::Item> {
        return self.filter(|line| line.is_empty());
    }

    fn non_empty(self) -> impl Iterator<Item = Self::Item> {
        return self.filter(|line| !line.is_empty());
    }
}
