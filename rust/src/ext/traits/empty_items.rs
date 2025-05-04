pub trait EmptyItems: Iterator {
    fn empty(self) -> impl Iterator<Item = Self::Item>;
    fn non_empty(self) -> impl Iterator<Item = Self::Item>;
}
