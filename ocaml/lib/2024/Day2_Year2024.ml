open Shared

let is_valid_report_alternative levels =
  let is_invalid_diff diff =
    let abs_diff = Int.abs diff in
    abs_diff > 3 || abs_diff = 0
  in
  let no_tail = List.tl levels in
  let no_head = List.rev (List.tl (List.rev levels)) in
  let diffs = List.map2 (fun prev next -> prev - next) no_tail no_head in
  let any_invalid = List.length (List.filter is_invalid_diff diffs) > 0 in
  let all_negative = List.length (List.filter (fun d -> d > 0) diffs) = 0 in
  let all_positive = List.length (List.filter (fun d -> d < 0) diffs) = 0 in
  (not any_invalid) && (all_negative || all_positive)

type report_direction = Increasing | Decreasing

let is_valid_report report =
  let rec inner levels previous_direction =
    match levels with
    | [] -> true
    | [ _ ] -> true
    | l1 :: l2 :: rest -> (
        let current_direction =
          if l1 - l2 > 0 then Increasing else Decreasing
        in
        let distance = Int.abs (l1 - l2) in
        let distance_exceeded = distance > 3 || distance == 0 in
        if distance_exceeded then false
        else
          match previous_direction with
          | None -> (inner [@tailcall]) (l2 :: rest) (Some current_direction)
          | Some d ->
              if current_direction != d then false
              else (inner [@tailcall]) (l2 :: rest) previous_direction)
  in
  inner report None

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
