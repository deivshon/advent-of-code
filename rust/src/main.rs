pub mod ext;
pub mod problems;
pub mod year_2020;

use std::time::SystemTime;
use std::{fs, path::Path, process};

use clap::Parser;

use problems::{Solution, SOLUTIONS};

#[derive(Parser)]
#[command(about, long_about = None)]
struct Arguments {
    #[arg(short, long)]
    year: Option<i32>,
    #[arg(short, long)]
    day: Option<i32>,
    #[arg(short, long)]
    part: Option<i32>,
}

fn filter_from_option<T: PartialEq + 'static>(value: Option<T>) -> Box<dyn Fn(T) -> bool> {
    match value {
        None => Box::new(|_| true),
        Some(value) => Box::new(move |compared| value == compared),
    }
}

fn main() {
    let args = Arguments::parse();

    let matches_year = filter_from_option(args.year);
    let matches_day = filter_from_option(args.day);
    let matches_part = filter_from_option(args.part);

    let wanted_solutions = SOLUTIONS
        .iter()
        .filter(|solution| {
            matches_year(solution.year) && matches_day(solution.day) && matches_part(solution.part)
        })
        .collect::<Vec<&Solution>>();

    if wanted_solutions.is_empty() {
        eprintln!("No solutions found for the given criteria.");
        process::exit(1)
    }

    wanted_solutions.iter().for_each(|solution| {
        let input_file_path = format!("./../puzzle-inputs/{}/{}.txt", solution.year, solution.day);
        let input_file = Path::new(&input_file_path);
        let puzzle_input: String = match fs::read_to_string(input_file) {
            Ok(input) => input,
            Err(e) => {
                eprintln!(
                    "{}/{} P{} | could not read puzzle input: {}",
                    solution.year, solution.day, solution.part, e
                );
                return;
            }
        };

        let start = SystemTime::now();
        let result = (solution.solution)(puzzle_input);
        let time_taken = match start.elapsed() {
            Ok(time_taken) => time_taken,
            Err(e) => {
                eprintln!(
                    "{}/{} P{} | could not compute elapsed time: {}",
                    solution.year, solution.day, solution.part, e
                );
                return;
            }
        };

        let correctness_indicator = match (&result, solution.expected) {
            (Ok(result), Some(expected)) => {
                if result == expected {
                    String::from("✅")
                } else {
                    String::from("❌")
                }
            }
            (_, _) => String::from("❓"),
        };

        let time_taken_pretty = {
            if time_taken.as_micros() < 2500 {
                format!("{}μs", time_taken.as_micros())
            } else if time_taken.as_millis() < 2500 {
                format!("{}ms", time_taken.as_millis())
            } else {
                format!("{}s", time_taken.as_secs())
            }
        };

        let expected_string: Option<String> = match (&result, solution.expected) {
            (Ok(result), Some(expected)) => {
                if result != expected {
                    Some(format!(" (expected {})", expected))
                } else {
                    None
                }
            }
            (_, _) => None,
        };

        println!(
            "{} | {}/{} P{}: {:6} | {}{}",
            correctness_indicator,
            solution.year,
            solution.day,
            solution.part,
            time_taken_pretty,
            match result {
                Ok(s) => s,
                Err(e) => format!("error computing solution: {}", e),
            },
            expected_string.unwrap_or_default()
        )
    });
}
