type solution = {
  solution_fn : string list -> (string, string) result;
  day : int;
  year : int;
  solution_number : int;
  expected_solution : string option;
}

let date_of execution =
  Printf.sprintf "%s/%s | %s"
    (string_of_int execution.year)
    (string_of_int execution.day)
    (string_of_int execution.solution_number)

let execute_solution execution =
  let execute =
    try
      let execution_date = date_of execution in
      let puzzle_input =
        Utils.get_puzzle_input_lines ~year:execution.year ~day:execution.day
      in
      let solution = execution.solution_fn puzzle_input in
      match solution with
      | Ok v -> (
          match execution.expected_solution with
          | Some expected ->
              if v = expected then Printf.printf "%s: %s\n" execution_date v
              else
                Printf.printf "%s: %s !!! (expected %s)\n" execution_date v
                  expected
          | None ->
              Printf.printf "%s: %s ??? (no expected solution specified)\n"
                execution_date v)
      | Error err ->
          Printf.printf "%s: could not obtain solution: %s\n" execution_date err
    with exc ->
      let execution_date = date_of execution in
      Printf.printf "%s: errored with exception: %s\n" execution_date
        (Printexc.to_string exc)
  in
  execute;
  flush stdout
