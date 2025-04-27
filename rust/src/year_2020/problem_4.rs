use anyhow::Result;
use thiserror::Error;

use crate::ext;

const CENTIMETERS_SUFFIX: &str = "cm";
const INCHES_SUFFIX: &str = "in";

enum HeightUnit {
    Centimeters,
    Inches,
}
struct Height {
    value: i32,
    unit: Option<HeightUnit>,
}

const VALID_EYE_COLOR: &[&str] = &["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

struct Passport {
    birth_year: Option<i32>,
    issue_year: Option<i32>,
    expiration_year: Option<i32>,
    height: Option<Height>,
    hair_color: Option<String>,
    eye_color: Option<String>,
    passport_id: Option<String>,
    country_id: Option<String>,
}

impl Passport {
    fn has_required_fields(&self) -> bool {
        return self.birth_year.is_some()
            && self.issue_year.is_some()
            && self.expiration_year.is_some()
            && self.height.is_some()
            && self.hair_color.is_some()
            && self.eye_color.is_some()
            && self.passport_id.is_some();
    }

    fn valid_birth_year(&self) -> bool {
        return match self.birth_year {
            Some(birth_year) => (1920..=2002).contains(&birth_year),
            None => false,
        };
    }

    fn valid_issue_year(&self) -> bool {
        return match self.issue_year {
            Some(issue_year) => (2010..=2020).contains(&issue_year),
            None => false,
        };
    }

    fn valid_expiration_year(&self) -> bool {
        return match self.expiration_year {
            Some(expiration_year) => (2020..=2030).contains(&expiration_year),
            None => false,
        };
    }

    fn valid_height(&self) -> bool {
        return match &self.height {
            Some(height) => match height.unit {
                Some(HeightUnit::Centimeters) => 150 <= height.value && height.value <= 193,
                Some(HeightUnit::Inches) => 59 <= height.value && height.value <= 76,
                None => false,
            },
            None => false,
        };
    }

    fn valid_hair_color(&self) -> bool {
        let is_valid_hex_char = ext::char::ascii_within(&[(48, 57), (97, 102)]);

        return match &self.hair_color {
            Some(hair_color) => {
                return hair_color.starts_with("#")
                    && hair_color.chars().filter(is_valid_hex_char).count() == 6;
            }
            None => false,
        };
    }

    fn valid_eye_color(&self) -> bool {
        return match &self.eye_color {
            Some(color) => VALID_EYE_COLOR.contains(&color.as_str()),
            None => false,
        };
    }

    fn valid_passport_id(&self) -> bool {
        return match &self.passport_id {
            Some(passport_id) => passport_id.len() == 9,
            None => false,
        };
    }

    fn is_valid(&self) -> bool {
        return self.valid_birth_year()
            && self.valid_issue_year()
            && self.valid_expiration_year()
            && self.valid_height()
            && self.valid_hair_color()
            && self.valid_eye_color()
            && self.valid_passport_id();
    }
}

#[derive(Error, Debug)]
enum PassportParseError {
    #[error("unknown data code {0}")]
    UnknownDataCode(String),
    #[error("wrong data format for pair {0}")]
    WrongPairFormat(String),
    #[error("could not parse value {0}")]
    ValueParseError(String),
}

fn passport_numeric(value: &str) -> Result<i32, PassportParseError> {
    return str::parse::<i32>(value).map_err(|_| PassportParseError::ValueParseError(value.into()));
}

fn passport_height(value: &str) -> Result<Height, PassportParseError> {
    let unit: Option<HeightUnit>;
    if value.ends_with(CENTIMETERS_SUFFIX) {
        unit = Some(HeightUnit::Centimeters);
    } else if value.ends_with(INCHES_SUFFIX) {
        unit = Some(HeightUnit::Inches);
    } else {
        unit = None
    }

    let digits_only = match unit {
        Some(HeightUnit::Centimeters) => value.trim_end_matches(CENTIMETERS_SUFFIX),
        Some(HeightUnit::Inches) => value.trim_end_matches(INCHES_SUFFIX),
        None => value,
    };
    let value = str::parse::<i32>(digits_only)
        .map_err(|_| PassportParseError::ValueParseError(value.into()))?;

    return Ok(Height { value, unit });
}

impl TryFrom<&str> for Passport {
    type Error = PassportParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let data_pairs: Vec<&str> = value.lines().flat_map(|line| line.split(" ")).collect();

        let mut passport = Passport {
            birth_year: None,
            country_id: None,
            expiration_year: None,
            height: None,
            hair_color: None,
            eye_color: None,
            issue_year: None,
            passport_id: None,
        };

        for pair in data_pairs {
            let pair_items: Vec<&str> = pair.split(":").collect();
            if pair_items.len() != 2 {
                return Err(PassportParseError::WrongPairFormat(pair.into()));
            }

            let data_code = pair_items[0];
            let value = pair_items[1];

            match data_code {
                "byr" => passport.birth_year = Some(passport_numeric(value)?),
                "iyr" => passport.issue_year = Some(passport_numeric(value)?),
                "eyr" => passport.expiration_year = Some(passport_numeric(value)?),
                "hgt" => passport.height = Some(passport_height(value)?),
                "hcl" => passport.hair_color = Some(value.into()),
                "ecl" => passport.eye_color = Some(value.into()),
                "pid" => passport.passport_id = Some(value.into()),
                "cid" => passport.country_id = Some(value.into()),
                unknown_data_code => {
                    return Err(PassportParseError::UnknownDataCode(
                        unknown_data_code.into(),
                    ))
                }
            }
        }

        return Ok(passport);
    }
}

fn count_valid(input: String, is_valid: impl Fn(&Passport) -> bool) -> Result<String> {
    let passports: Vec<Passport> = Result::from_iter(
        input
            .split("\n\n")
            .filter(|section| !section.is_empty())
            .map(Passport::try_from)
            .filter(Result::is_ok),
    )?;

    return Ok(passports
        .iter()
        .filter(|&passport| is_valid(passport))
        .count()
        .to_string());
}

pub fn part_1(input: String) -> Result<String> {
    return count_valid(input, |passport| passport.has_required_fields());
}

pub fn part_2(input: String) -> Result<String> {
    return count_valid(input, |passport| {
        passport.has_required_fields() && passport.is_valid()
    });
}
