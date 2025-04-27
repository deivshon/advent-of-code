use anyhow::{Error, Result};
use thiserror::Error;

struct Entry {
    n1: i32,
    n2: i32,
    letter: char,
    password: String,
}

#[derive(Error, Debug)]
enum EntryParseError {
    #[error("found {0} sections, expected 3")]
    SectionsMismatch(usize),
    #[error("found {0} number sections, expected 2")]
    NumbersSectionsMismatch(usize),
    #[error("found {0} letters sections, expected 2")]
    LettersSectionsMismatch(usize),
    #[error("no letter in letter section")]
    NoLetter,
}

fn parse_input_line(line: &str) -> Result<Entry> {
    let split = line.split(" ").collect::<Vec<&str>>();
    if split.len() != 3 {
        return Err(EntryParseError::SectionsMismatch(split.len()).into());
    }

    let numbers_section = split[0];
    let letter_section = split[1];
    let password = split[2];

    let numbers_split = numbers_section.split("-").collect::<Vec<&str>>();
    if numbers_split.len() != 2 {
        return Err(EntryParseError::NumbersSectionsMismatch(numbers_split.len()).into());
    }
    let n1 = numbers_split[0].parse::<i32>()?;
    let n2 = numbers_split[1].parse::<i32>()?;

    if letter_section.len() != 2 {
        return Err(EntryParseError::LettersSectionsMismatch(numbers_split.len()).into());
    }
    let letter = match letter_section.chars().next() {
        Some(l) => l,
        None => return Err(EntryParseError::NoLetter.into()),
    };

    return Ok(Entry {
        n1,
        n2,
        letter,
        password: String::from(password),
    });
}

#[derive(Error, Debug)]
enum InputParseError {
    #[error("error parsing line {0}: {1}")]
    EntryParseError(usize, Error),
}

fn parse_input(input: String) -> Result<Vec<Entry>> {
    return Result::from_iter(
        input
            .lines()
            .filter(|line| !line.is_empty())
            .enumerate()
            .map(|(idx, line)| {
                parse_input_line(line)
                    .map_err(|err| InputParseError::EntryParseError(idx + 1, err).into())
            }),
    );
}

pub fn part_1(input: String) -> Result<String> {
    let entries = parse_input(input)?;
    let valid: Vec<&Entry> = entries
        .iter()
        .filter(|entry| {
            let letter_count = entry
                .password
                .chars()
                .filter(|c| *c == entry.letter)
                .count();

            return match i32::try_from(letter_count) {
                Ok(letter_count) => letter_count >= entry.n1 && letter_count <= entry.n2,
                Err(_) => false,
            };
        })
        .collect();

    return Ok(valid.len().to_string());
}

pub fn part_2(input: String) -> Result<String> {
    let entries = parse_input(input)?;
    let valid: Vec<&Entry> = entries
        .iter()
        .filter(|entry| {
            let (n1, n2) = match (usize::try_from(entry.n1), usize::try_from(entry.n2)) {
                (Ok(n1), Ok(n2)) => (n1, n2),
                (_, _) => return false,
            };

            return entry
                .password
                .chars()
                .enumerate()
                .filter(|(idx, char)| (idx + 1 == n1 || idx + 1 == n2) && *char == entry.letter)
                .count()
                == 1;
        })
        .collect();

    return Ok(valid.len().to_string());
}
