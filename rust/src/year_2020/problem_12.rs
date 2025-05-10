use anyhow::{anyhow, bail, ensure, Result};
use thiserror::Error;

use crate::ext::traits::empty_items::EmptyItems;

enum Instruction {
    North(i32),
    South(i32),
    East(i32),
    West(i32),
    Left(i32),
    Right(i32),
    Forward(i32),
}

#[derive(PartialEq, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

enum TurnDirection {
    Right,
    Left,
}

const CLOCKWISE_DIRECTIONS: &[Direction; 4] = &[
    Direction::North,
    Direction::East,
    Direction::South,
    Direction::West,
];

const COUNTER_CLOCKWISE_DIRECTIONS: &[Direction; 4] = &[
    Direction::North,
    Direction::West,
    Direction::South,
    Direction::East,
];

impl Direction {
    pub fn turn(&self, to: TurnDirection, by: i32) -> Result<Direction> {
        let directions = match to {
            TurnDirection::Left => COUNTER_CLOCKWISE_DIRECTIONS,
            TurnDirection::Right => CLOCKWISE_DIRECTIONS,
        };

        let start_index = &directions
            .iter()
            .enumerate()
            .find(|(_, direction)| direction == &self)
            .ok_or(anyhow!("could not find matching direction in directions"))?
            .0;

        let result_index = match by {
            90 => start_index + 1,
            180 => start_index + 2,
            270 => start_index + 3,
            unhandled_degrees => bail!(
                "can't turn by {} degrees, only 90/180/270 values allowed",
                unhandled_degrees
            ),
        } % 4;

        ensure!(result_index < directions.len());

        return Ok(directions[result_index]);
    }
}

#[derive(Error, Debug)]
enum InputParseError {
    #[error("empty line: {1}, at index: {0}")]
    EmptyLine(usize, String),
    #[error("malformed associated value: {1}, at index: {0}")]
    MalformedAssociatedValue(usize, String),
    #[error("unknown instruction: {1}, in line: {2}, at index: {0}")]
    UnknownInstruction(usize, String, String),
}

fn parse_input(input: String) -> Result<Vec<Instruction>, InputParseError> {
    return Result::from_iter(input.lines().non_empty().enumerate().map(|(idx, line)| {
        if line.is_empty() {
            return Err(InputParseError::EmptyLine(idx, line.into()));
        }

        let associated_value = match str::parse::<i32>(&line[1..]) {
            Ok(value) => value,
            Err(_) => return Err(InputParseError::MalformedAssociatedValue(idx, line.into())),
        };
        let instruction = match &line[0..1] {
            "N" => Instruction::North(associated_value),
            "S" => Instruction::South(associated_value),
            "E" => Instruction::East(associated_value),
            "W" => Instruction::West(associated_value),
            "L" => Instruction::Left(associated_value),
            "R" => Instruction::Right(associated_value),
            "F" => Instruction::Forward(associated_value),
            unknown_instruction => {
                return Err(InputParseError::UnknownInstruction(
                    idx,
                    unknown_instruction.into(),
                    line.into(),
                ))
            }
        };

        return Ok(instruction);
    }));
}

pub fn part_1(input: String) -> Result<String> {
    let instructions = parse_input(input)?;

    let mut coordinates = (0, 0);
    let mut direction = Direction::East;
    for instruction in instructions {
        match instruction {
            Instruction::North(units) => coordinates.0 += units,
            Instruction::East(units) => coordinates.1 += units,
            Instruction::South(units) => coordinates.0 -= units,
            Instruction::West(units) => coordinates.1 -= units,
            Instruction::Forward(units) => match direction {
                Direction::North => coordinates.0 += units,
                Direction::East => coordinates.1 += units,
                Direction::South => coordinates.0 -= units,
                Direction::West => coordinates.1 -= units,
            },
            Instruction::Left(degrees) => {
                direction = direction.turn(TurnDirection::Left, degrees)?
            }
            Instruction::Right(degrees) => {
                direction = direction.turn(TurnDirection::Right, degrees)?
            }
        }
    }

    return Ok((coordinates.0.abs() + coordinates.1.abs()).to_string());
}

fn relative_turn(
    position: (i32, i32),
    direction: TurnDirection,
    degrees: i32,
) -> Result<(i32, i32)> {
    let invert: bool;
    let move_factors: (i32, i32);
    let consider_direction: bool;

    match degrees {
        90 => {
            invert = true;
            move_factors = (-1, 1);
            consider_direction = true;
        }
        180 => {
            invert = false;
            move_factors = (-1, -1);
            consider_direction = false;
        }
        270 => {
            invert = true;
            move_factors = (1, -1);
            consider_direction = true;
        }
        unhandled_degrees => bail!(
            "can't relative turn by {} degrees, only 90/180/270 values allowed",
            unhandled_degrees
        ),
    };

    let direction_coeff = if consider_direction {
        match direction {
            TurnDirection::Left => -1,
            TurnDirection::Right => 1,
        }
    } else {
        1
    };

    return Ok(if invert {
        (
            position.1 * move_factors.0 * direction_coeff,
            position.0 * move_factors.1 * direction_coeff,
        )
    } else {
        (
            position.0 * move_factors.0 * direction_coeff,
            position.1 * move_factors.1 * direction_coeff,
        )
    });
}

pub fn part_2(input: String) -> Result<String> {
    let instructions = parse_input(input)?;

    let mut waypoint_offset = (1, 10);
    let mut coordinates = (0, 0);
    for instruction in instructions {
        match instruction {
            Instruction::North(units) => waypoint_offset.0 += units,
            Instruction::East(units) => waypoint_offset.1 += units,
            Instruction::South(units) => waypoint_offset.0 -= units,
            Instruction::West(units) => waypoint_offset.1 -= units,
            Instruction::Left(degrees) => {
                waypoint_offset = relative_turn(waypoint_offset, TurnDirection::Left, degrees)?
            }
            Instruction::Right(degrees) => {
                waypoint_offset = relative_turn(waypoint_offset, TurnDirection::Right, degrees)?
            }
            Instruction::Forward(times) => {
                coordinates.0 += waypoint_offset.0 * times;
                coordinates.1 += waypoint_offset.1 * times;
            }
        }
    }

    return Ok((coordinates.0.abs() + coordinates.1.abs()).to_string());
}
