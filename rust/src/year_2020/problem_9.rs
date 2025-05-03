use std::collections::{HashSet, VecDeque};

use anyhow::{bail, ensure, Result};

fn parse_input(input: &str) -> Result<Vec<i64>> {
    return Result::from_iter(
        input
            .lines()
            .filter(|line| !line.is_empty())
            .map(|raw_number| str::parse::<i64>(raw_number).map_err(|err| err.into()))
            .collect::<Vec<Result<i64>>>(),
    );
}

fn two_sum(target: i64, pool: &HashSet<i64>) -> Option<(i64, i64)> {
    for first in pool.iter() {
        if let Some(second) = pool.get(&(target - *first)) {
            return Some((*first, *second));
        }
    }

    return None;
}

const PREAMBLE_LEN: usize = 25;
pub fn part_1(input: String) -> Result<String> {
    let numbers = parse_input(&input)?;

    let mut set_pool: HashSet<i64> = HashSet::new();
    let mut vec_pool: VecDeque<i64> = VecDeque::new();

    let mut first_invalid: Option<i64> = None;
    for n in numbers {
        let pool_size = vec_pool.len();
        if pool_size < PREAMBLE_LEN {
            set_pool.insert(n);
            vec_pool.push_back(n);
            continue;
        }

        if two_sum(n, &set_pool).is_none() {
            first_invalid = Some(n);
            break;
        }

        let vec_removed = match vec_pool.pop_front() {
            Some(value) => value,
            None => bail!("no value to remove from vec"),
        };
        if !vec_pool.contains(&vec_removed) {
            let set_removed = set_pool.remove(&vec_removed);
            ensure!(set_removed);
        }
        set_pool.insert(n);
        vec_pool.push_back(n);
    }

    if let Some(first_invalid) = first_invalid {
        return Ok(first_invalid.to_string());
    }

    bail!("all numbers in the sequence are valid")
}

pub fn part_2(input: String) -> Result<String> {
    let numbers = parse_input(&input)?;
    let first_invalid = str::parse::<i64>(part_1(input)?.as_str())?;

    for idx in 0..numbers.len() {
        let mut sum: i64 = 0;
        let mut sum_addends: Vec<i64> = Vec::new();

        let mut sum_idx: usize = idx;
        while sum_idx < numbers.len() && sum < first_invalid {
            sum += numbers[sum_idx];
            sum_addends.push(numbers[sum_idx]);

            sum_idx += 1;
        }

        ensure!(sum == sum_addends.iter().sum());

        if sum == first_invalid {
            let min_addend = sum_addends.iter().min();
            let max_addend = sum_addends.iter().max();

            if let (Some(min_addend), Some(max_addend)) = (min_addend, max_addend) {
                return Ok((min_addend + max_addend).to_string());
            }

            bail!("min_addend or max_addend is none after sum evaluation")
        }
    }

    bail!("no valid contiguous set of numbers found")
}
