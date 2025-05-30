use std::collections::HashSet;

use anyhow::{bail, Result};

fn parse_input(input: String) -> Vec<i32> {
    return input
        .split("\n")
        .map(str::parse::<i32>)
        .filter_map(Result::ok)
        .collect();
}

const TARGET: i32 = 2020;

pub fn part_1(input: String) -> Result<String> {
    let numbers = parse_input(input);

    let mut seen: HashSet<i32> = HashSet::new();
    for current in numbers.iter() {
        match seen.get(&(TARGET - current)) {
            Some(corresponding) => return Ok((current * corresponding).to_string()),
            None => seen.insert(*current),
        };
    }

    bail!("no valid pair found");
}

pub fn part_2(input: String) -> Result<String> {
    let parsed = parse_input(input);
    let numbers: HashSet<&i32> = HashSet::from_iter(parsed.iter());

    for n1 in numbers.iter() {
        for n2 in numbers.iter() {
            if let Some(n3) = numbers.get(&(TARGET - *n1 - *n2)) {
                return Ok((*n1 * *n2 * *n3).to_string());
            }
        }
    }

    bail!("no valid triplet found");
}
