use std::fmt::Display;

use anyhow::Result;
use thiserror::Error;

use crate::ext::matrix::{self, WalkUntilOpts};
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

    pub fn stabilized(&self, immediate_adjecency: bool) -> Self {
        let mut rows: Vec<Vec<LayoutCell>> = self.rows.clone();

        let mut stabilized = false;
        while !stabilized {
            (rows, stabilized) = stabilization_step(rows, immediate_adjecency);
        }

        return SeatsLayout { rows };
    }
}

fn stabilization_step(
    previous: Vec<Vec<LayoutCell>>,
    immediate_adjacency: bool,
) -> (Vec<Vec<LayoutCell>>, bool) {
    let needed_to_free_seat = if immediate_adjacency { 4 } else { 5 };

    let mut already_stabilized = true;
    let mut updated_layout: Vec<Vec<LayoutCell>> = Vec::new();
    for row_idx in 0..previous.len() {
        let mut updated_row: Vec<LayoutCell> = Vec::new();

        for col_idx in 0..previous[row_idx].len() {
            match previous[row_idx][col_idx] {
                LayoutCell::Seat(occupied) => {
                    let adjacents_occupied = match immediate_adjacency {
                        true => previous.adjacent_to((row_idx, col_idx)),
                        false => {
                            find_first_visible(&previous, (row_idx as isize, col_idx as isize))
                        }
                    }
                    .iter()
                    .filter(|cell| matches!(cell, Some(LayoutCell::Seat(true))))
                    .count();

                    if occupied && adjacents_occupied >= needed_to_free_seat {
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

const ALL_DIRECTION_STEPS: [(isize, isize); 8] = [
    (-1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
    (1, 0),
    (1, -1),
    (0, -1),
    (-1, -1),
];

fn find_first_visible(
    previous: &[Vec<LayoutCell>],
    starting_position: (isize, isize),
) -> [Option<&LayoutCell>; 8] {
    let mut result: [Option<&LayoutCell>; 8] = [const { None }; 8];
    for step_idx in 0..ALL_DIRECTION_STEPS.len() {
        result[step_idx] = matrix::walk_until(
            previous,
            |cell| !matches!(*cell, LayoutCell::Floor),
            WalkUntilOpts {
                start_position: starting_position,
                step: ALL_DIRECTION_STEPS[step_idx],
            },
        );
    }

    return result;
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
    let stabilized = layout.stabilized(true);

    return Ok(stabilized.count_seats(true).to_string());
}

pub fn part_2(input: String) -> Result<String> {
    let layout = SeatsLayout::try_from(input)?;
    let stabilized = layout.stabilized(false);

    return Ok(stabilized.count_seats(true).to_string());
}
