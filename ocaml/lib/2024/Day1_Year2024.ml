let parse_puzzle_input raw_input =
  raw_input
  |> StringList.remove_empty
  |> List.map (String.split_on_char ' ')
  |> List.map (fun pair -> List.filter (fun s -> String.length s > 0) pair)
  |> List.map (fun pair ->
         (int_of_string (List.nth pair 0), int_of_string (List.nth pair 1)))
  |> List.fold_left
       (fun acc pair -> (fst pair :: fst acc, snd pair :: snd acc))
       ([], [])

let first_solution raw_input =
  let puzzle_input = parse_puzzle_input raw_input in
  let sorted_input = Tuple.map2 (List.sort Int.compare) puzzle_input in
  let step acc x y = acc + abs (x - y) in
  Ok (List.fold_left2 step 0 (fst sorted_input) (snd sorted_input))
  |> Result.map string_of_int

let second_solution raw_input =
  let puzzle_input = parse_puzzle_input raw_input in
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
