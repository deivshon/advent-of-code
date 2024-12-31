let parse_puzzle_input raw_input =
  raw_input |> StringList.remove_empty |> List.map CharList.from_string

let analyze_plot map point =
  let seen_table = Hashtbl.create 256 in
  let mark_seen p = Hashtbl.add seen_table p true in
  let is_unseen p = Hashtbl.find_opt seen_table p |> Option.is_none in
  let get_already_seen () = HashtblExt.keys seen_table in

  let plot_type = Matrix.element_at point map in
  let is_within_bounds = Matrix.is_within_bounds map in

  let process_points points =
    let map_single point =
      let near = Matrix.near ~is_within_bounds point in
      let near_out_of_map = 4 - List.length near in
      let near_same, near_different =
        near |> List.partition (fun p -> Matrix.element_at p map = plot_type)
      in
      let near_same_unseen = List.filter is_unseen near_same in
      (near_same_unseen, near_out_of_map + List.length near_different)
    in
    let raw_mapped = List.map map_single points in
    let new_points =
      raw_mapped |> List.concat_map fst |> ListExt.remove_duplicates ( = )
    in
    let added_perimeter =
      let step acc (_, perimeter) = acc + perimeter in
      List.fold_left step 0 raw_mapped
    in
    (new_points, added_perimeter)
  in
  let rec inner map points perimeter =
    match points with
    | [] -> (get_already_seen (), perimeter)
    | _ ->
        let new_points, added_perimeter = process_points points in
        let _mark_current_points = List.iter mark_seen points in
        (inner [@tailcall]) map new_points (perimeter + added_perimeter)
  in
  inner map [ point ] 0

let compute_total_price map =
  let analyzed_table = Hashtbl.create 1024 in
  let mark_analyzed_point p = Hashtbl.add analyzed_table p true in
  let is_already_analyzed p = Hashtbl.mem analyzed_table p in

  let init_single ri ci =
    let p = (ri, ci) in
    if is_already_analyzed p then None
    else
      let region, perimeter = analyze_plot map p in
      let _mark_region = List.iter mark_analyzed_point region in
      Some (List.length region, perimeter)
  in
  Matrix.init_flatten init_single map
  |> List.filter_map Fun.id
  |> List.fold_left (fun acc (area, perimeter) -> acc + (area * perimeter)) 0

let first_solution raw_input =
  raw_input
  |> parse_puzzle_input
  |> compute_total_price
  |> string_of_int
  |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 12;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "1437300";
    };
  ]
