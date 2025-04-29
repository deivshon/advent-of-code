use std::collections::{HashMap, HashSet};

use anyhow::Result;

type Answers = Vec<char>;

fn parse_input(input: String) -> Vec<Vec<Answers>> {
    return input
        .split("\n\n")
        .filter(|line| !line.is_empty())
        .map(|group_answers| {
            group_answers
                .lines()
                .map(|line| line.chars().collect::<Answers>())
                .collect::<Vec<Answers>>()
        })
        .collect();
}

pub fn part_1(input: String) -> Result<String> {
    let groups_answers = parse_input(input);

    let mut anyone_answered_yes_sum: usize = 0;
    for group_answers in groups_answers {
        let mut answered_yes: HashSet<char> = HashSet::new();

        for answers in group_answers {
            for answer in answers {
                answered_yes.insert(answer);
            }
        }

        anyone_answered_yes_sum += answered_yes.len();
    }

    return Ok(anyone_answered_yes_sum.to_string());
}

pub fn part_2(input: String) -> Result<String> {
    let groups_answers = parse_input(input);

    let mut everyone_answered_yes_sum: usize = 0;
    for group_answers in groups_answers {
        let mut yes_counts: HashMap<char, usize> = HashMap::new();

        for answers in &group_answers {
            for answer in answers {
                match yes_counts.get(answer) {
                    Some(current) => yes_counts.insert(*answer, current + 1),
                    None => yes_counts.insert(*answer, 1),
                };
            }
        }

        everyone_answered_yes_sum += yes_counts
            .into_iter()
            .filter(|(_, count)| *count == group_answers.len())
            .count();
    }

    return Ok(everyone_answered_yes_sum.to_string());
}
