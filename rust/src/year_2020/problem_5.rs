use anyhow::{bail, Result};
use thiserror::Error;

enum Direction {
    Back,
    Right,
    Front,
    Left,
}

#[derive(Error, Debug)]
enum InputParseError {
    #[error("unknown character found: {0}")]
    UnknownCharacter(char),
}

fn parse_input(input: String) -> Result<Vec<Vec<Direction>>> {
    let mut seats: Vec<Vec<Direction>> = Vec::new();

    for line in input.lines().filter(|line| !line.is_empty()) {
        let mut seat: Vec<Direction> = Vec::new();

        for char in line.chars() {
            match char {
                'B' => seat.push(Direction::Back),
                'R' => seat.push(Direction::Right),
                'F' => seat.push(Direction::Front),
                'L' => seat.push(Direction::Left),
                unknown_character => {
                    return Err(InputParseError::UnknownCharacter(unknown_character).into())
                }
            }
        }

        seats.push(seat)
    }

    return Ok(seats);
}

#[derive(Error, Debug)]
enum SeatIdConversionError {
    #[error("undefined row after completion, got bounds {0} to {1}")]
    UndefinedRow(i32, i32),
    #[error("undefined column after completion, got bounds {0} to {1}")]
    UndefinedColumn(i32, i32),
}

const ROWS: i32 = 127;
const COLUMNS: i32 = 7;
fn seat_id_conversion(directions: Vec<Direction>) -> Result<i32> {
    let mut row_start = 0;
    let mut row_end = ROWS;
    let mut col_start = 0;
    let mut col_end = COLUMNS;

    for direction in directions {
        match direction {
            Direction::Back => {
                row_start += (row_end - row_start + 1) / 2;
            }
            Direction::Front => {
                row_end -= (row_end - row_start + 1) / 2;
            }
            Direction::Left => {
                col_end -= (col_end - col_start + 1) / 2;
            }
            Direction::Right => {
                col_start += (col_end - col_start + 1) / 2;
            }
        }
    }

    if row_start != row_end {
        return Err(SeatIdConversionError::UndefinedRow(row_start, row_end).into());
    }
    if col_start != col_end {
        return Err(SeatIdConversionError::UndefinedColumn(col_start, col_end).into());
    }

    return Ok(row_start * 8 + col_start);
}

pub fn part_1(input: String) -> Result<String> {
    let seat_ids: Vec<i32> =
        Result::from_iter(parse_input(input)?.into_iter().map(seat_id_conversion))?;

    match seat_ids.iter().max() {
        Some(max_id) => return Ok(max_id.to_string()),
        None => bail!("no seats specified, can't find max"),
    }
}

pub fn part_2(input: String) -> Result<String> {
    let mut seat_ids: Vec<i32> =
        Result::from_iter(parse_input(input)?.into_iter().map(seat_id_conversion))?;
    seat_ids.sort();

    for idx in 1..(seat_ids.len()) {
        if seat_ids[idx] - seat_ids[idx - 1] == 2 {
            return Ok((seat_ids[idx] - 1).to_string());
        }
    }

    bail!("no suitable seat found")
}
