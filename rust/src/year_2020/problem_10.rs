use std::collections::HashMap;

use anyhow::{anyhow, bail, ensure, Result};

fn parse_input(input: String) -> Result<Vec<i64>> {
    return Result::from_iter(
        input
            .lines()
            .filter(|line| !line.is_empty())
            .map(|raw_number| str::parse::<i64>(raw_number).map_err(|err| err.into()))
            .collect::<Vec<Result<i64>>>(),
    );
}

pub fn part_1(input: String) -> Result<String> {
    let mut numbers = parse_input(input)?;
    numbers.push(0);
    numbers.sort();

    let mut one_differences: i64 = 0;
    let mut three_differences: i64 = 0;
    for idx in 0..(numbers.len() - 1) {
        let current = numbers[idx];
        let next = numbers[idx + 1];

        match next - current {
            1 => one_differences += 1,
            2 => continue,
            3 => three_differences += 1,
            invalid => bail!("invalid difference detected: {}, at line: {}", invalid, idx),
        }
    }

    return Ok((one_differences * (three_differences + 1)).to_string());
}

fn lookup_or_compute(
    source_idx: usize,
    numbers: &Vec<i64>,
    lookup_table: &mut HashMap<i64, i64>,
) -> Result<i64> {
    let cached = lookup_table.get(&numbers[source_idx]).cloned();
    let combinations = match cached {
        Some(value) => value,
        None => compute_combinations(numbers[source_idx], numbers, lookup_table)?,
    };
    lookup_table.insert(numbers[source_idx], combinations);

    return Ok(combinations);
}

fn compute_combinations(
    source: i64,
    numbers: &Vec<i64>,
    lookup_table: &mut HashMap<i64, i64>,
) -> Result<i64> {
    let source_idx = numbers
        .iter()
        .position(|n| *n == source)
        .ok_or(anyhow!("could not find combinations source"))?;

    if source_idx == numbers.len() - 1 {
        return Ok(1);
    };

    let first_near_idx = source_idx + 1;
    let second_near_idx = source_idx + 2;
    let third_near_idx = source_idx + 3;

    ensure!(numbers[first_near_idx] - source <= 3);

    let first_near_combinations = lookup_or_compute(first_near_idx, numbers, lookup_table)?;

    if second_near_idx >= numbers.len() || numbers[second_near_idx] - source > 3 {
        return Ok(first_near_combinations);
    }

    let second_near_combinations = lookup_or_compute(second_near_idx, numbers, lookup_table)?;

    if third_near_idx >= numbers.len() || numbers[third_near_idx] - source > 3 {
        return Ok(first_near_combinations + second_near_combinations);
    }

    let third_near_combinations = lookup_or_compute(third_near_idx, numbers, lookup_table)?;

    return Ok(first_near_combinations + second_near_combinations + third_near_combinations);
}

pub fn part_2(input: String) -> Result<String> {
    let mut numbers = parse_input(input)?;
    numbers.push(0);
    numbers.sort();

    return Ok(compute_combinations(0, &numbers, &mut HashMap::new())?.to_string());
}
