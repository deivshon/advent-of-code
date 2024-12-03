let (solutions : Aoc.Execution.solution list) =
  Aoc.Day1_Year2024.solutions @ Aoc.Day2_Year2024.solutions

let () = List.iter Aoc.Execution.execute_solution solutions
