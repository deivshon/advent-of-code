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

let get_relevant_orderings orderings update =
  let step acc o =
    if List.mem (fst o) update && List.mem (snd o) update then o :: acc else acc
  in
  List.fold_left step [] orderings |> List.rev

let pages_compare orderings p1 p2 =
  if List.mem (p1, p2) orderings then -1
  else if List.mem (p2, p1) orderings then 1
  else 0

let is_valid_update orderings update =
  let ordered = List.sort (pages_compare orderings) update in
  List.for_all2 ( = ) update ordered

let take_middle l = List.nth l (List.length l / 2)

let first_solution raw_input =
  let orderings, updates = parse_puzzle_input raw_input in
  let valid_updates = List.filter (is_valid_update orderings) updates in
  List.map take_middle valid_updates
  |> List.fold_left ( + ) 0
  |> string_of_int
  |> Result.ok

let second_solution raw_input =
  let take_middle l = List.nth l (List.length l / 2) in
  let orderings, updates = parse_puzzle_input raw_input in
  let invalid_updates =
    let filter_fn update = not (is_valid_update orderings update) in
    List.filter filter_fn updates
  in
  invalid_updates
  |> List.map (List.sort (pages_compare orderings))
  |> List.map take_middle
  |> List.fold_left ( + ) 0
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
    {
      solution_fn = second_solution;
      day = 5;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "5169";
    };
  ]
