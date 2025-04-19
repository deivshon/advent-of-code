use std::collections::HashSet;

fn parse_input(input: String) -> Vec<i32> {
    return input
        .split("\n")
        .map(|n| str::parse::<i32>(n))
        .filter_map(Result::ok)
        .collect();
}

const TARGET: i32 = 2020;

pub fn part_1(input: String) -> Option<String> {
    let numbers = parse_input(input);

    let mut seen: HashSet<i32> = HashSet::new();
    for current in numbers.iter() {
        match seen.get(&(TARGET - current)) {
            Some(corresponding) => return Some((current * corresponding).to_string()),
            None => seen.insert(*current),
        };
    }

    return None;
}

pub fn part_2(input: String) -> Option<String> {
    let parsed = parse_input(input);
    let numbers: HashSet<&i32> = HashSet::from_iter(parsed.iter());

    for n1 in numbers.iter() {
        for n2 in numbers.iter() {
            match numbers.get(&(TARGET - *n1 - *n2)) {
                Some(n3) => return Some((*n1 * *n2 * *n3).to_string()),
                None => (),
            }
        }
    }

    return None;
}
