use crate::ext::traits::adjacent::Adjacent;

impl<T> Adjacent<8> for Vec<Vec<T>> {
    type Item = T;
    type Coordinates = (usize, usize);

    fn adjacent_to(&self, coordinates: Self::Coordinates) -> [Option<&Self::Item>; 8] {
        let (row_idx, col_idx) = coordinates;

        let top_left = if row_idx > 0 && col_idx > 0 {
            Some(&self[row_idx - 1][col_idx - 1])
        } else {
            None
        };
        let top_center = if row_idx > 0 {
            Some(&self[row_idx - 1][col_idx])
        } else {
            None
        };
        let top_right = if row_idx > 0 && col_idx < self[row_idx].len() - 1 {
            Some(&self[row_idx - 1][col_idx + 1])
        } else {
            None
        };
        let middle_left = if col_idx > 0 {
            Some(&self[row_idx][col_idx - 1])
        } else {
            None
        };
        let middle_right = if col_idx < self[row_idx].len() - 1 {
            Some(&self[row_idx][col_idx + 1])
        } else {
            None
        };
        let bottom_left = if row_idx < self.len() - 1 && col_idx > 0 {
            Some(&self[row_idx + 1][col_idx - 1])
        } else {
            None
        };
        let bottom_center = if row_idx < self.len() - 1 {
            Some(&self[row_idx + 1][col_idx])
        } else {
            None
        };
        let bottom_right = if row_idx < self.len() - 1 && col_idx < self[row_idx].len() - 1 {
            Some(&self[row_idx + 1][col_idx + 1])
        } else {
            None
        };

        return [
            top_left,
            top_center,
            top_right,
            middle_left,
            middle_right,
            bottom_left,
            bottom_center,
            bottom_right,
        ];
    }
}
