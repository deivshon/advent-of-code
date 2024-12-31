open Aoc.Execution

let (all_solutions : solution list) =
  Aoc.Day1_Year2024.solutions
  @ Aoc.Day2_Year2024.solutions
  @ Aoc.Day3_Year2024.solutions
  @ Aoc.Day4_Year2024.solutions
  @ Aoc.Day5_Year2024.solutions
  @ Aoc.Day6_Year2024.solutions
  @ Aoc.Day7_Year2024.solutions
  @ Aoc.Day8_Year2024.solutions
  @ Aoc.Day9_Year2024.solutions
  @ Aoc.Day10_Year2024.solutions
  @ Aoc.Day11_Year2024.solutions
  @ Aoc.Day12_Year2024.solutions

let usage_message = "aoc -y <solution_year> -d <solution_day>"
let year_arg = ref ""
let day_arg = ref ""
let part_arg = ref ""

let speclist =
  [
    ("-y", Arg.Set_string year_arg, "Year");
    ("-d", Arg.Set_string day_arg, "Day");
    ("-p", Arg.Set_string part_arg, "Part");
  ]

let () =
  let _ = Arg.parse speclist (fun _ -> ()) usage_message in
  let requested_solutions =
    if String.length year_arg.contents > 0 && String.length day_arg.contents > 0
    then
      try
        let year = int_of_string year_arg.contents in
        let day = int_of_string day_arg.contents in
        let parts =
          match part_arg.contents with
          | "" -> [ 1; 2 ]
          | _ -> [ int_of_string part_arg.contents ]
        in
        let matches_arguments solution =
          solution.year = year
          && solution.day = day
          && List.mem solution.solution_number parts
        in
        match List.find_all matches_arguments all_solutions with
        | [] -> Error "no solution found for specified arguments"
        | ls -> Ok (Some ls)
      with
      | Not_found -> Error "no solution found for the given arguments"
      | _ -> Error (Printf.sprintf "arguments could not be parsed")
    else if
      String.length year_arg.contents > 0 || String.length day_arg.contents > 0
    then Error "provide both day and year to specify a solution"
    else Ok None
  in
  match requested_solutions with
  | Ok requested -> (
      match requested with
      | Some requested -> List.iter execute_solution requested
      | None -> List.iter execute_solution all_solutions)
  | Error e -> Printf.eprintf "error: %s\n" e
