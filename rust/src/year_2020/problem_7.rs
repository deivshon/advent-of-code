use std::collections::HashMap;

use anyhow::Result;
use thiserror::Error;

use crate::static_regex;

static_regex!(outer_re, r"^([a-z]+ [a-z]+) bags contain (.+)\.$");
static_regex!(inner_re, r"(\d+) ([a-z]+ [a-z]+) bags?");

const TARGET_BAG: &str = "shiny gold";

#[derive(Error, Debug)]
enum InputParseError {
    #[error("no subject match for line: {0}")]
    NoSubjectMatch(String),
    #[error("mismatched subject captures: got {0} but expected 3 for line: {1}")]
    MismatchedOuterCaptures(usize, String),
    #[error("mismatched content captures: got {0} but expected 3 for line: {1}")]
    MismatchedInnerCaptures(usize, String),
    #[error("could not parse bags amount integer: {0} in line {1}")]
    BagsAmountParseError(String, String),
}

fn parse_input(input: String) -> Result<HashMap<String, Vec<(String, i32)>>> {
    let mut bags_map: HashMap<String, Vec<(String, i32)>> = HashMap::new();

    let outer_re = outer_re();
    let inner_re = inner_re();
    for line in input.lines() {
        if line.is_empty() {
            continue;
        }

        let outer_captures = outer_re
            .captures(line)
            .ok_or(InputParseError::NoSubjectMatch(line.into()))?;

        if outer_captures.len() != 3 {
            return Err(InputParseError::MismatchedOuterCaptures(
                outer_captures.len(),
                line.into(),
            )
            .into());
        }

        let bag = &outer_captures[1];
        let content = &outer_captures[2];

        let mut identified_contents: Vec<(String, i32)> = Vec::new();
        for inner_captures in inner_re.captures_iter(content) {
            if inner_captures.len() != 3 {
                return Err(InputParseError::MismatchedInnerCaptures(
                    inner_captures.len(),
                    line.into(),
                )
                .into());
            }

            let amount = str::parse::<i32>(&inner_captures[1]).map_err(|_| {
                InputParseError::BagsAmountParseError((&inner_captures[1]).into(), line.into())
            })?;
            let contained_bag = &inner_captures[2];

            identified_contents.push((contained_bag.into(), amount));
        }

        bags_map.insert(bag.into(), identified_contents);
    }

    return Ok(bags_map);
}

pub fn get_target_containers(
    target_bag: &str,
    bags_map: HashMap<String, Vec<(String, i32)>>,
) -> usize {
    fn populate_answers_map(
        bag: &str,
        target_bag: &str,
        bags_map: &HashMap<String, Vec<(String, i32)>>,
        answers_map: &mut HashMap<String, bool>,
    ) -> bool {
        if let Some(existing_answer) = answers_map.get(bag) {
            return *existing_answer;
        }

        let contained = match bags_map.get(bag) {
            Some(contained) => contained,
            None => return false,
        };

        let mut contains_target = false;
        for (contained_bag, _) in contained {
            if contained_bag == target_bag {
                answers_map.insert(bag.into(), true);
                contains_target = true;
            } else {
                let indirectly_contained =
                    populate_answers_map(contained_bag, target_bag, bags_map, answers_map);

                if indirectly_contained {
                    answers_map.insert(bag.into(), true);
                    contains_target = true;
                } else {
                    answers_map.insert(contained_bag.into(), false);
                }
            }
        }

        return contains_target;
    }

    let mut answers_map: HashMap<String, bool> = HashMap::new();
    for (bag, _) in bags_map.iter() {
        _ = populate_answers_map(bag, target_bag, &bags_map, &mut answers_map);
    }

    return answers_map
        .iter()
        .filter(|(_, contains_target)| **contains_target)
        .count();
}

pub fn part_1(input: String) -> Result<String> {
    let bags_map = parse_input(input)?;
    return Ok(get_target_containers(TARGET_BAG, bags_map).to_string());
}

fn get_target_contained(target_bag: &str, bags_map: &HashMap<String, Vec<(String, i32)>>) -> i32 {
    let contained = match bags_map.get(target_bag) {
        Some(contained) => contained,
        None => return 0,
    };
    if contained.is_empty() {
        return 0;
    }

    return contained
        .iter()
        .map(|(bag, amount)| amount + amount * get_target_contained(bag, bags_map))
        .sum();
}

pub fn part_2(input: String) -> Result<String> {
    let bags_map = parse_input(input)?;
    return Ok(get_target_contained(TARGET_BAG, &bags_map).to_string());
}
