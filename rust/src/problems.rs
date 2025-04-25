use crate::year_2020;

pub struct Solution {
    pub year: i32,
    pub day: i32,
    pub part: i32,
    pub solution: fn(String) -> Option<String>,
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
];
