pub trait Adjacent<const N: usize> {
    type Item;
    type Coordinates: Copy;

    fn adjacent_to(&self, coordinates: Self::Coordinates) -> [Option<&Self::Item>; N];
}
