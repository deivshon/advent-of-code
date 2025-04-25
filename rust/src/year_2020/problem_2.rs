struct Entry {
    n1: i32,
    n2: i32,
    letter: char,
    password: String,
}

fn parse_input_line(line: &str) -> Option<Entry> {
    let split = line.split(" ").collect::<Vec<&str>>();
    if split.len() != 3 {
        return None;
    }

    let numbers_section = split[0];
    let letter_section = split[1];
    let password = split[2];

    let numbers_split = numbers_section.split("-").collect::<Vec<&str>>();
    if numbers_split.len() != 2 {
        return None;
    }
    let n1 = numbers_split[0].parse::<i32>().ok()?;
    let n2 = numbers_split[1].parse::<i32>().ok()?;

    if letter_section.len() != 2 {
        return None;
    }
    let letter = letter_section.chars().next()?;

    return Some(Entry {
        n1,
        n2,
        letter,
        password: String::from(password),
    });
}

fn parse_input(input: String) -> Vec<Entry> {
    return input.split("\n").filter_map(parse_input_line).collect();
}

pub fn part_1(input: String) -> Option<String> {
    let entries = parse_input(input);
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

    return Some(valid.len().to_string());
}

pub fn part_2(input: String) -> Option<String> {
    let entries = parse_input(input);
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

    return Some(valid.len().to_string());
}
