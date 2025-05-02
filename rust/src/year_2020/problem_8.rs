use std::collections::HashSet;

use anyhow::{anyhow, bail, Result};
use thiserror::Error;

use crate::static_regex;

#[derive(Debug)]
enum Instruction {
    NoOperation(i32),
    Accumulator(i32),
    Jump(i32),
}

struct Program {
    instructions: Vec<Instruction>,
}

struct ProgramExecutionReport {
    accumulator: i32,
    ended: bool,
    executed_instructions: HashSet<usize>,
}

impl Program {
    pub fn execute(&self, fix: Option<usize>) -> Result<ProgramExecutionReport> {
        if self.instructions.is_empty() {
            return Ok(ProgramExecutionReport {
                accumulator: 0,
                ended: true,
                executed_instructions: HashSet::new(),
            });
        }

        let mut accumulator: i32 = 0;
        let mut executed_instructions: HashSet<usize> = HashSet::new();

        let mut instruction_index: usize = 0;
        while !executed_instructions.contains(&instruction_index) {
            if instruction_index == self.instructions.len() {
                return Ok(ProgramExecutionReport {
                    accumulator,
                    ended: true,
                    executed_instructions,
                });
            }
            if instruction_index > self.instructions.len() {
                return Err(anyhow!("instruction index invalid: {}", instruction_index));
            }

            executed_instructions.insert(instruction_index);

            let base_instruction = &self.instructions[instruction_index];
            let instruction = match fix {
                Some(fix_index) => match (fix_index == instruction_index, base_instruction) {
                    (true, Instruction::NoOperation(value)) => &Instruction::Jump(*value),
                    (true, Instruction::Jump(value)) => &Instruction::NoOperation(*value),
                    (true, non_fixable) => {
                        return Err(anyhow!(
                            "invalid fix {}: found non fixable instruction: {:?}",
                            fix_index,
                            non_fixable,
                        ))
                    }
                    (false, _) => base_instruction,
                },
                None => base_instruction,
            };

            match instruction {
                Instruction::NoOperation(_) => instruction_index += 1,
                Instruction::Accumulator(value) => {
                    instruction_index += 1;
                    accumulator += value;
                }
                Instruction::Jump(value) => instruction_index += *value as usize,
            }
        }

        return Ok(ProgramExecutionReport {
            accumulator,
            ended: false,
            executed_instructions,
        });
    }

    pub fn execute_with_fix(&self) -> Result<ProgramExecutionReport> {
        for possible_fix in self.execute(None)?.executed_instructions {
            if matches!(self.instructions[possible_fix], Instruction::Accumulator(_)) {
                continue;
            }

            let execution = self.execute(Some(possible_fix))?;
            if execution.ended {
                return Ok(execution);
            }
        }

        bail!("all possible fixes where applied, but none resulted in a loop-free execution")
    }
}

#[derive(Error, Debug)]
enum InstructionParseError {
    #[error("line did not match the expected instruction format: {0}")]
    Format(String),
    #[error("invalid instruction type in line: {0}")]
    Type(String),
    #[error("invalid numeric value in line: {0}")]
    Value(String),
}

static_regex!(instruction_regex, r"(nop|acc|jmp) ([+-]\d+)");

impl TryFrom<String> for Program {
    type Error = InstructionParseError;

    fn try_from(value: String) -> std::result::Result<Self, Self::Error> {
        let raw_instructions = value.lines().filter(|line| !line.is_empty());
        let instructions = Result::from_iter(raw_instructions.map(|raw| {
            let captures = instruction_regex()
                .captures(raw)
                .ok_or(InstructionParseError::Format(raw.into()))?;
            if captures.len() != 3 {
                return Err(InstructionParseError::Format(raw.into()));
            }

            let associated_value = match str::parse::<i32>(&captures[2]) {
                Ok(value) => value,
                Err(_) => return Err(InstructionParseError::Value(raw.into())),
            };
            let instruction = match &captures[1] {
                "nop" => Instruction::NoOperation(associated_value),
                "acc" => Instruction::Accumulator(associated_value),
                "jmp" => Instruction::Jump(associated_value),
                _ => return Err(InstructionParseError::Type(raw.into())),
            };

            return Ok(instruction);
        }))?;

        return Ok(Program { instructions });
    }
}

pub fn part_1(input: String) -> Result<String> {
    let program = Program::try_from(input)?;
    return Ok(program.execute(None)?.accumulator.to_string());
}

pub fn part_2(input: String) -> Result<String> {
    let program = Program::try_from(input)?;
    return Ok(program.execute_with_fix()?.accumulator.to_string());
}
