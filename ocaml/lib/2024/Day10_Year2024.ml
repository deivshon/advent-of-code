let parse_puzzle_input raw_input =
  raw_input
  |> StringList.remove_empty
  |> List.map CharList.from_string
  |> List.map (List.map CharExt.to_string)
  |> List.map (List.map int_of_string)

let get_next_points is_within_bounds map (ri, ci) =
  let height = Matrix.element_at (ri, ci) map in
  Matrix.near ~is_within_bounds (ri, ci)
  |> List.filter (fun coords -> Matrix.element_at coords map = height + 1)

let get_trailhead_score ~unique_trails is_within_bounds map (ri, ci) =
  let rec inner height acc =
    if height = 9 then List.length acc
    else
      let new_acc =
        acc
        |> List.concat_map (get_next_points is_within_bounds map)
        |> if unique_trails then Fun.id else ListExt.remove_duplicates ( = )
      in
      (inner [@tailcall]) (height + 1) new_acc
  in
  inner 0 [ (ri, ci) ]

let get_scores_sum ~unique_trails map =
  let is_within_bounds = Matrix.is_within_bounds map in
  let starting_points = Matrix.find_all_index (( = ) 0) map in
  starting_points
  |> List.map (get_trailhead_score ~unique_trails is_within_bounds map)
  |> IntList.sum

let solution ~unique_trails raw_input =
  parse_puzzle_input raw_input
  |> get_scores_sum ~unique_trails
  |> string_of_int
  |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = solution ~unique_trails:false;
      day = 10;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "825";
    };
    {
      solution_fn = solution ~unique_trails:true;
      day = 10;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "1805";
    };
  ]
