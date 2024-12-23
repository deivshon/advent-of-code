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

let ( ++ ) a b =
  let a_str = string_of_int a in
  let b_str = string_of_int b in
  String.concat "" [ a_str; b_str ] |> int_of_string

let is_possible_equation ~include_concatenation equation =
  let result, numbers = equation in
  let step acc right =
    let map_single left =
      match include_concatenation with
      | true -> [ right * left; right + left; left ++ right ]
      | false -> [ right * left; right + left ]
    in
    match acc with
    | [] -> [ right ]
    | acc -> List.map map_single acc |> List.flatten
  in
  let all_possible_results = List.fold_left step [] numbers in
  List.mem result all_possible_results

let solution ~include_concatenation raw_input =
  parse_puzzle_input raw_input
  |> ListExt.par_filter (is_possible_equation ~include_concatenation)
  |> List.map fst
  |> IntList.sum
  |> string_of_int
  |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = solution ~include_concatenation:false;
      day = 7;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "2314935962622";
    };
    {
      solution_fn = solution ~include_concatenation:true;
      day = 7;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "401477450831495";
    };
  ]
