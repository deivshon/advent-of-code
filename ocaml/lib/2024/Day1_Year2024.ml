open Syntax

let parse_puzzle_input raw_input =
  let parsed_pair_results =
    raw_input
    |> StringList.remove_empty
    |> List.map (String.split_on_char ' ')
    |> List.map (fun pair -> List.filter (fun s -> String.length s > 0) pair)
    |> List.map (fun pair ->
           match List.length pair with
           | 2 ->
               Ok
                 ( int_of_string (List.nth pair 0),
                   int_of_string (List.nth pair 1) )
           | _ -> Error "found pair with more than 2 elements")
  in
  let any_errored =
    List.exists
      (fun parsed_pair_result ->
        match parsed_pair_result with Ok _ -> false | Error _ -> true)
      parsed_pair_results
  in
  let parsed_lists =
    List.fold_right
      (fun pair_result acc ->
        match pair_result with Ok v -> v :: acc | Error _ -> acc)
      parsed_pair_results []
    |> List.fold_left
         (fun acc pair -> (fst pair :: fst acc, snd pair :: snd acc))
         ([], [])
    |> Tuple.map2 List.rev
  in
  match any_errored with
  | true -> Error "the input was not properly formatted"
  | false -> Ok parsed_lists

let first_solution raw_input =
  let* puzzle_input = parse_puzzle_input raw_input in
  let sorted_input = Tuple.map2 (List.sort Int.compare) puzzle_input in
  let step acc a b = acc + abs (a - b) in
  Ok (List.fold_left2 step 0 (fst sorted_input) (snd sorted_input))
  |> Result.map string_of_int

let second_solution raw_input =
  let* puzzle_input = parse_puzzle_input raw_input in
  let step acc fst_v =
    acc + (fst_v * ListExt.count (fun snd_v -> snd_v = fst_v) (snd puzzle_input))
  in
  Ok (List.fold_left step 0 (fst puzzle_input)) |> Result.map string_of_int

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 1;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "1603498";
    };
    {
      solution_fn = second_solution;
      day = 1;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "25574739";
    };
  ]
