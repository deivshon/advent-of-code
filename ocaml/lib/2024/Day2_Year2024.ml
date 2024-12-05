open Shared

let parse_puzzle_input raw_input =
  raw_input
  |> remove_empty_strings
  |> List.map (String.split_on_char ' ')
  |> List.map remove_empty_strings
  |> List.map (fun l -> List.map int_of_string l)

type report_direction = Increasing | Decreasing

let is_valid_report_part1 report =
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

let is_valid_report_part2 report =
  List.init (List.length report) Fun.id
  |> List.map (fun idx -> report |> without_element_at idx)
  |> List.exists is_valid_report_part1

let solution ~report_validity_fn raw_input =
  let puzzle_input = parse_puzzle_input raw_input in
  let successful_reports =
    List.map report_validity_fn puzzle_input
    |> List.filter Fun.id
    |> List.length
  in
  Ok (string_of_int successful_reports)

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = solution ~report_validity_fn:is_valid_report_part1;
      day = 2;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "371";
    };
    {
      solution_fn = solution ~report_validity_fn:is_valid_report_part2;
      day = 2;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "426";
    };
  ]
