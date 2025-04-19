use crate::year_2020;

pub struct Solution {
    pub year: i32,
    pub day: i32,
    pub part: i32,
    pub solution: fn(String) -> Option<String>,
}

pub static SOLUTIONS: &'static [Solution] = &[
    Solution {
        year: 2020,
        day: 1,
        part: 1,
        solution: year_2020::problem_1::part_1,
    },
    Solution {
        year: 2020,
        day: 1,
        part: 2,
        solution: year_2020::problem_1::part_2,
    },
];
