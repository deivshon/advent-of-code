let parse_puzzle_input raw_input =
  let parse_line line =
    let split_line = String.split_on_char ':' line in
    let equation_result = List.nth split_line 0 |> int_of_string in
    let numbers =
      List.nth split_line 1
      |> String.split_on_char ' '
      |> List.filter StringExt.not_empty
      |> List.map StringExt.digits_only
      |> List.map int_of_string
    in
    (equation_result, numbers)
  in
  raw_input |> StringList.remove_empty |> List.map parse_line

let is_possible_equation equation =
  let result, numbers = equation in
  let step acc x =
    match acc with
    | [] -> [ x ]
    | acc -> List.map (fun y -> [ y * x; y + x ]) acc |> List.flatten
  in
  let all_possible_results = List.fold_left step [] numbers in
  List.mem result all_possible_results

let first_solution raw_input =
  parse_puzzle_input raw_input
  |> List.filter is_possible_equation
  |> List.map fst
  |> IntList.sum
  |> string_of_int
  |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 7;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "2314935962622";
    };
  ]
