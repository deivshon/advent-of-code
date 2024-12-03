open Shared

type report_direction = Increasing | Decreasing

let is_valid_report reports =
  let rec inner reports previous_direction =
    match reports with
    | [] -> true
    | [ _ ] -> true
    | r1 :: r2 :: rest -> (
        let current_direction =
          if r1 - r2 > 0 then Increasing else Decreasing
        in
        let distance = Int.abs (r1 - r2) in
        let distance_exceeded = distance > 3 || distance == 0 in
        if distance_exceeded then false
        else
          match previous_direction with
          | None -> (inner [@tailcall]) (r2 :: rest) (Some current_direction)
          | Some d ->
              if current_direction != d then false
              else (inner [@tailcall]) (r2 :: rest) previous_direction)
  in
  inner reports None

let parse_puzzle_input raw_input =
  raw_input
  |> remove_empty_strings
  |> List.map (String.split_on_char ' ')
  |> List.map remove_empty_strings
  |> List.map (fun ls -> List.map int_of_string ls)

let first_solution raw_input =
  let puzzle_input = parse_puzzle_input raw_input in
  let successful_reports =
    List.map is_valid_report puzzle_input
    |> List.filter (fun report_result -> report_result)
    |> List.length
  in
  Ok (string_of_int successful_reports)

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 2;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "371";
    };
  ]