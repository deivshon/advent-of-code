open Shared

let parse_page_orderings raw_page_ordering =
  raw_page_ordering
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char '|')
  |> List.map (List.map int_of_string)
  |> List.map (fun l -> (List.hd l, List.nth l 1))

let parse_updates raw_updates =
  raw_updates
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)

let parse_puzzle_input raw_input =
  let raw_page_ordering, raw_updates =
    let split_input =
      raw_input
      |> remove_empty_strings_at_end
      |> String.concat "\n"
      |> Str.split (Str.regexp "\n\n")
    in
    (List.hd split_input, List.nth split_input 1)
  in
  let orderings = parse_page_orderings raw_page_ordering in
  let updates = parse_updates raw_updates in
  (orderings, updates)

let is_valid_update orderings update =
  let ordering_invalidates_update update ordering =
    let rec inner update snd_seen =
      match update with
      | [] -> false
      | x :: xs ->
          if x = fst ordering && snd_seen then true
          else (inner [@tailcall]) xs (snd_seen || x = snd ordering)
    in
    inner update false
  in
  not (List.exists (ordering_invalidates_update update) orderings)

let first_solution raw_input =
  let take_middle l = List.nth l (List.length l / 2) in
  let orderings, updates = parse_puzzle_input raw_input in
  let valid_updates = List.filter (is_valid_update orderings) updates in
  List.map take_middle valid_updates
  |> List.fold_left (fun acc v -> acc + v) 0
  |> string_of_int
  |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 5;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "6242";
    };
  ]
