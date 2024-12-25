let parse_puzzle_input raw_input =
  raw_input |> StringList.remove_empty |> List.map CharList.from_string

let get_antennas_syms world_map =
  let init_cell world_map ri ci =
    let cell_content = Matrix.element_at (ri, ci) world_map in
    match cell_content with '.' -> None | _ -> Some cell_content
  in
  Matrix.init_flatten (init_cell world_map) world_map
  |> List.filter Option.is_some
  |> List.map Option.get
  |> ListExt.remove_duplicates ( = )

let get_antenna_pair_antinodes ?(coeff = 2) a1 a2 =
  let row1, col1 = a1 in
  let row2, col2 = a2 in
  let antinode_row1 = (coeff * row1) - ((coeff - 1) * row2) in
  let antinode_row2 = (coeff * row2) - ((coeff - 1) * row1) in
  let antinode_col1 = (coeff * col1) - ((coeff - 1) * col2) in
  let antinode_col2 = (coeff * col2) - ((coeff - 1) * col1) in
  [ (antinode_row1, antinode_col1); (antinode_row2, antinode_col2) ]

let get_antenna_antinodes antenna_index same_sym_indexes =
  same_sym_indexes
  |> List.map (get_antenna_pair_antinodes antenna_index)
  |> List.flatten

let get_antenna_antinodes_line antenna_index same_sym_indexes world_map =
  let is_within_map = Matrix.is_within_bounds world_map in
  let map_single a1 a2 =
    let rec inner a1 a2 acc coeff =
      let current_antinodes =
        get_antenna_pair_antinodes a1 a2 ~coeff |> List.filter is_within_map
      in
      match List.length current_antinodes with
      | 0 -> acc |> List.rev |> List.flatten
      | _ -> (inner [@tailcall]) a1 a2 (current_antinodes :: acc) (coeff + 1)
    in
    inner a1 a2 [] 1
  in
  same_sym_indexes |> List.map (map_single antenna_index) |> List.flatten

let get_sym_antinodes world_map ~whole_line sym =
  let indexes = Matrix.find_all_index (Char.equal sym) world_map in
  let rec inner indexes acc =
    match indexes with
    | [] -> acc |> List.rev |> List.flatten
    | x :: xs ->
        let antinodes =
          match whole_line with
          | false -> get_antenna_antinodes x xs
          | true -> get_antenna_antinodes_line x xs world_map
        in
        (inner [@tailcall]) xs (antinodes :: acc)
  in
  inner indexes []
  |>
  let is_within_map = Matrix.is_within_bounds world_map in
  List.filter is_within_map

let get_antinodes ~whole_line world_map =
  let syms = get_antennas_syms world_map in
  List.map (get_sym_antinodes world_map ~whole_line) syms
  |> List.flatten
  |> ListExt.remove_duplicates ( = )

let solution ~whole_line raw_input =
  parse_puzzle_input raw_input
  |> get_antinodes ~whole_line
  |> List.length
  |> string_of_int
  |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = solution ~whole_line:false;
      day = 8;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "228";
    };
    {
      solution_fn = solution ~whole_line:true;
      day = 8;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "766";
    };
  ]
