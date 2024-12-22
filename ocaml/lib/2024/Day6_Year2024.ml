open Shared

type direction = Up | Right | Down | Left

exception Invalid_world_map of string

let direction_turned_right direction =
  match direction with Up -> Right | Right -> Down | Down -> Left | Left -> Up

let parse_puzzle_input raw_input =
  raw_input
  |> remove_empty_strings
  |> List.map (fun s -> List.init (String.length s) (String.get s))

let find_start_position world_map =
  let is_starting_position char = char = '^' in
  Matrix.find_index is_starting_position world_map

let without_start_position world_map =
  Matrix.map (fun char -> if char != '^' then char else '.') world_map

let next_position position_row position_col direction =
  match direction with
  | Up -> (position_row - 1, position_col)
  | Right -> (position_row, position_col + 1)
  | Down -> (position_row + 1, position_col)
  | Left -> (position_row, position_col - 1)

let walk ?(new_obstacle = None) position direction world_map =
  let rows, cols = Matrix.get_dimensions world_map in
  let rec inner position direction world_map acc previous_position =
    let position_row, position_col = position in
    if
      position_row < 0
      || position_row >= rows
      || position_col < 0
      || position_col >= cols
    then (acc, None)
    else
      let current =
        Matrix.element_at ~row:position_row ~col:position_col world_map
      in
      let position_is_new_obstacle =
        match new_obstacle with
        | Some obstacle_position -> position = obstacle_position
        | None -> false
      in
      match (current, position_is_new_obstacle) with
      | '#', _ | _, true ->
          ( (if List.length acc > 0 then List.tl acc else []),
            Some previous_position )
      | '.', false ->
          let next_position =
            next_position position_row position_col direction
          in
          (inner [@tailcall]) next_position direction world_map
            (position :: acc) position
      | _ -> raise (Invalid_world_map "contains non-standard blocks")
  in
  inner position direction world_map [] position

let get_unique_walked ?(new_obstacle = None) position direction world_map =
  let visited_table = Hashtbl.create 5000 in
  let rec inner position direction world_map acc c =
    let walked, new_position =
      walk ~new_obstacle position direction world_map
    in
    let new_direction = direction_turned_right direction in
    let walked_with_direction =
      let add_direction direction position =
        let position_row, position_col = position in
        (position_row, position_col, direction)
      in
      List.map (add_direction direction) walked
    in
    let new_acc = walked_with_direction @ acc in
    let entered_loop =
      List.exists (Hashtbl.mem visited_table) walked_with_direction
    in
    let _mut_insert_in_visited =
      walked_with_direction
      |> List.iter (fun position -> Hashtbl.add visited_table position true)
    in
    match (entered_loop, new_position) with
    | true, _ | false, None ->
        let is_same_position p1 p2 =
          let p1_row, p1_col, _ = p1 in
          let p2_row, p2_col, _ = p2 in
          p1_row = p2_row && p1_col = p2_col
        in
        let unique_positions =
          new_acc
          |> list_remove_duplicates is_same_position
          |> List.map (fun (p_row, p_col, _) -> (p_row, p_col))
        in
        (unique_positions, entered_loop)
    | false, Some new_position ->
        (inner [@tailcall]) new_position new_direction world_map new_acc (c + 1)
  in
  inner position direction world_map [] 0

let first_solution raw_input =
  let raw_world_map = parse_puzzle_input raw_input in
  let start_position = find_start_position raw_world_map in
  let world_map = without_start_position raw_world_map in
  let walked, _ = get_unique_walked start_position Up world_map in
  Ok (List.length walked) |> Result.map string_of_int

let second_solution raw_input =
  let raw_world_map = parse_puzzle_input raw_input in
  let start_position = find_start_position raw_world_map in
  let world_map = without_start_position raw_world_map in
  let walked, _ = get_unique_walked start_position Up world_map in
  let enters_loop start_position start_direction world_map new_obstacle =
    let _, entered_loop =
      get_unique_walked ~new_obstacle:(Some new_obstacle) start_position
        start_direction world_map
    in
    entered_loop
  in
  List.filter (fun position -> position <> start_position) walked
  |> List.map (enters_loop start_position Up world_map)
  |> list_count Fun.id
  |> Result.ok
  |> Result.map string_of_int

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 6;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "4883";
    };
    {
      solution_fn = second_solution;
      day = 6;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "1655";
    };
  ]
