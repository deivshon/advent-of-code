open Aoc.Execution

let (solutions : solution list) =
  Aoc.Day1_Year2024.solutions
  @ Aoc.Day2_Year2024.solutions
  @ Aoc.Day3_Year2024.solutions
  @ Aoc.Day4_Year2024.solutions
  @ Aoc.Day5_Year2024.solutions
  @ Aoc.Day6_Year2024.solutions
  @ Aoc.Day7_Year2024.solutions

let usage_message = "aoc -y <solution_year> -d <solution_day>"
let year_arg = ref ""
let day_arg = ref ""

let speclist =
  [
    ("-y", Arg.Set_string year_arg, "Year");
    ("-d", Arg.Set_string day_arg, "Day");
  ]

let () =
  let _ = Arg.parse speclist (fun _ -> ()) usage_message in
  let requested_solutions =
    if String.length year_arg.contents > 0 && String.length day_arg.contents > 0
    then
      try
        let year = int_of_string year_arg.contents in
        let day = int_of_string day_arg.contents in
        match
          List.find_all (fun s -> s.year = year && s.day = day) solutions
        with
        | [] -> Error "no solution found for specified day and year"
        | ls -> Ok (Some ls)
      with
      | Not_found ->
          Error
            (Printf.sprintf "no solution found for day %s of year %s"
               day_arg.contents year_arg.contents)
      | _ -> Error (Printf.sprintf "arguments could not be parsed")
    else if
      String.length year_arg.contents > 0 || String.length day_arg.contents > 0
    then Error "provide both day and year to specify a solution"
    else Ok None
  in
  match requested_solutions with
  | Ok requested -> (
      match requested with
      | Some s -> List.iter execute_solution s
      | None -> List.iter execute_solution solutions)
  | Error e -> Printf.eprintf "error: %s" e
