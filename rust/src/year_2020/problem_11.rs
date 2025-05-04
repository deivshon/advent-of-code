use std::fmt::Display;

use anyhow::Result;
use thiserror::Error;

use crate::ext::traits::{adjacent::Adjacent, empty_items::EmptyItems};

#[derive(Clone, Debug)]
enum LayoutCell {
    Seat(bool),
    Floor,
}

struct SeatsLayout {
    rows: Vec<Vec<LayoutCell>>,
}

impl SeatsLayout {
    pub fn count_seats(&self, occupied: bool) -> usize {
        return self
            .rows
            .iter()
            .map(|row| {
                row.iter()
                    .filter(|cell| match cell {
                        LayoutCell::Seat(seat_occupied) => *seat_occupied == occupied,
                        _ => false,
                    })
                    .count()
            })
            .sum();
    }

    fn stabilization_step(&self, previous: Vec<Vec<LayoutCell>>) -> (Vec<Vec<LayoutCell>>, bool) {
        let mut already_stabilized = true;

        let mut updated_layout: Vec<Vec<LayoutCell>> = Vec::new();

        for row_idx in 0..previous.len() {
            let mut updated_row: Vec<LayoutCell> = Vec::new();

            for col_idx in 0..previous[row_idx].len() {
                match previous[row_idx][col_idx] {
                    LayoutCell::Seat(occupied) => {
                        let adjacents_occupied = previous
                            .adjacent_to((row_idx, col_idx))
                            .iter()
                            .filter(|cell| matches!(cell, Some(LayoutCell::Seat(true))))
                            .count();

                        if occupied && adjacents_occupied >= 4 {
                            updated_row.push(LayoutCell::Seat(false));
                            already_stabilized = false;
                        } else if !occupied && adjacents_occupied == 0 {
                            updated_row.push(LayoutCell::Seat(true));
                            already_stabilized = false;
                        } else {
                            updated_row.push(LayoutCell::Seat(occupied))
                        }
                    }
                    LayoutCell::Floor => updated_row.push(LayoutCell::Floor),
                }
            }

            updated_layout.push(updated_row);
        }

        return (updated_layout, already_stabilized);
    }

    pub fn stabilized(&self) -> Self {
        let mut rows: Vec<Vec<LayoutCell>> = self.rows.clone();

        let mut stabilized = false;
        while !stabilized {
            (rows, stabilized) = self.stabilization_step(rows);
        }

        return SeatsLayout { rows };
    }
}

impl Display for SeatsLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatted = String::new();

        for row in &self.rows {
            let mut formatted_row = String::new();

            for cell in row {
                match cell {
                    LayoutCell::Seat(false) => formatted_row.push('L'),
                    LayoutCell::Seat(true) => formatted_row.push('#'),
                    LayoutCell::Floor => formatted_row.push('.'),
                }
            }

            formatted.push_str(&formatted_row);
            formatted.push('\n');
        }

        return write!(f, "{}", formatted.trim_end());
    }
}

#[derive(Error, Debug)]
enum SeatsLayoutParseError {
    #[error("unknown cell: {0}")]
    UnknownCell(char),
    #[error("invalid row length: {0}, expected first row length: {1}")]
    InvalidRowLength(usize, usize),
}

impl TryFrom<String> for SeatsLayout {
    type Error = SeatsLayoutParseError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut rows: Vec<Vec<LayoutCell>> = Vec::new();

        for line in value.lines().non_empty() {
            let mut row: Vec<LayoutCell> = Vec::new();

            for char in line.chars() {
                match char {
                    'L' => row.push(LayoutCell::Seat(false)),
                    '#' => row.push(LayoutCell::Seat(true)),
                    '.' => row.push(LayoutCell::Floor),
                    unknown => return Err(SeatsLayoutParseError::UnknownCell(unknown)),
                }
            }

            if !rows.is_empty() && row.len() != rows[0].len() {
                return Err(SeatsLayoutParseError::InvalidRowLength(
                    row.len(),
                    rows[0].len(),
                ));
            }
            rows.push(row);
        }

        return Ok(SeatsLayout { rows });
    }
}

pub fn part_1(input: String) -> Result<String> {
    let layout = SeatsLayout::try_from(input)?;
    let stabilized = layout.stabilized();

    return Ok(stabilized.count_seats(true).to_string());
}
