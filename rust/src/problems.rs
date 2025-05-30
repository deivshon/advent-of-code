use anyhow::Result;

use crate::year_2020;

pub struct Solution {
    pub year: i32,
    pub day: i32,
    pub part: i32,
    pub solution: fn(String) -> Result<String>,
    pub expected: Option<&'static str>,
}

pub static SOLUTIONS: &[Solution] = &[
    Solution {
        year: 2020,
        day: 1,
        part: 1,
        solution: year_2020::problem_1::part_1,
        expected: Some("440979"),
    },
    Solution {
        year: 2020,
        day: 1,
        part: 2,
        solution: year_2020::problem_1::part_2,
        expected: Some("82498112"),
    },
    Solution {
        year: 2020,
        day: 2,
        part: 1,
        solution: year_2020::problem_2::part_1,
        expected: Some("517"),
    },
    Solution {
        year: 2020,
        day: 2,
        part: 2,
        solution: year_2020::problem_2::part_2,
        expected: Some("284"),
    },
    Solution {
        year: 2020,
        day: 3,
        part: 1,
        solution: year_2020::problem_3::part_1,
        expected: Some("211"),
    },
    Solution {
        year: 2020,
        day: 3,
        part: 2,
        solution: year_2020::problem_3::part_2,
        expected: Some("3584591857"),
    },
    Solution {
        year: 2020,
        day: 4,
        part: 1,
        solution: year_2020::problem_4::part_1,
        expected: Some("230"),
    },
    Solution {
        year: 2020,
        day: 4,
        part: 2,
        solution: year_2020::problem_4::part_2,
        expected: Some("156"),
    },
    Solution {
        year: 2020,
        day: 5,
        part: 1,
        solution: year_2020::problem_5::part_1,
        expected: Some("994"),
    },
    Solution {
        year: 2020,
        day: 5,
        part: 2,
        solution: year_2020::problem_5::part_2,
        expected: Some("741"),
    },
    Solution {
        year: 2020,
        day: 6,
        part: 1,
        solution: year_2020::problem_6::part_1,
        expected: Some("6714"),
    },
    Solution {
        year: 2020,
        day: 6,
        part: 2,
        solution: year_2020::problem_6::part_2,
        expected: Some("3435"),
    },
    Solution {
        year: 2020,
        day: 7,
        part: 1,
        solution: year_2020::problem_7::part_1,
        expected: Some("139"),
    },
    Solution {
        year: 2020,
        day: 7,
        part: 2,
        solution: year_2020::problem_7::part_2,
        expected: Some("58175"),
    },
    Solution {
        year: 2020,
        day: 8,
        part: 1,
        solution: year_2020::problem_8::part_1,
        expected: Some("1475"),
    },
    Solution {
        year: 2020,
        day: 8,
        part: 2,
        solution: year_2020::problem_8::part_2,
        expected: Some("1270"),
    },
    Solution {
        year: 2020,
        day: 9,
        part: 1,
        solution: year_2020::problem_9::part_1,
        expected: Some("27911108"),
    },
    Solution {
        year: 2020,
        day: 9,
        part: 2,
        solution: year_2020::problem_9::part_2,
        expected: Some("4023754"),
    },
    Solution {
        year: 2020,
        day: 10,
        part: 1,
        solution: year_2020::problem_10::part_1,
        expected: Some("2400"),
    },
    Solution {
        year: 2020,
        day: 10,
        part: 2,
        solution: year_2020::problem_10::part_2,
        expected: Some("338510590509056"),
    },
    Solution {
        year: 2020,
        day: 11,
        part: 1,
        solution: year_2020::problem_11::part_1,
        expected: Some("2438"),
    },
    Solution {
        year: 2020,
        day: 11,
        part: 2,
        solution: year_2020::problem_11::part_2,
        expected: Some("2174"),
    },
    Solution {
        year: 2020,
        day: 12,
        part: 1,
        solution: year_2020::problem_12::part_1,
        expected: Some("2297"),
    },
    Solution {
        year: 2020,
        day: 12,
        part: 2,
        solution: year_2020::problem_12::part_2,
        expected: Some("89984"),
    },
];
