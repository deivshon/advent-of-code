let element_at (ri, ci) m = List.nth (List.nth m ri) ci

exception Invalid_matrix of string

let get_dimensions m =
  let rows = List.length m in
  let get_cols_number m =
    let rec inner m expected_cols =
      match m with
      | [] -> ( match expected_cols with Some n -> Ok n | None -> Ok 0)
      | x :: xs -> (
          let row_cols = List.length x in
          match expected_cols with
          | Some n ->
              if n <> row_cols then
                Error "rows don't all have the same amount of columns"
              else (inner [@tailcall]) xs expected_cols
          | None -> (inner [@tailcall]) xs (Some row_cols))
    in
    inner m None
  in
  let cols = get_cols_number m in
  match cols with
  | Ok cols -> (rows, cols)
  | Error _ ->
      raise
        (Invalid_matrix "rows number did not always match with columns number")

let find_index f m =
  let rec inner f m ri =
    match m with
    | [] -> raise Not_found
    | x :: xs -> (
        match List.find_index f x with
        | Some matching_ci -> (ri, matching_ci)
        | None -> (inner [@tailcall]) f xs (ri + 1))
  in
  inner f m 0

let find_all_index f m =
  let rec inner f m ri acc =
    match m with
    | [] -> acc |> List.rev |> List.flatten
    | x :: xs ->
        let found_in_row =
          ListExt.find_all_index f x |> List.map (fun ci -> (ri, ci))
        in
        (inner [@tailcall]) f xs (ri + 1) (found_in_row :: acc)
  in
  inner f m 0 []

let map f m = List.map (List.map f) m

let init_flatten f m =
  let rows, cols = get_dimensions m in
  let rows_indexes = List.init rows Fun.id in
  let cols_indexes = List.init cols Fun.id in
  rows_indexes
  |> List.map (fun ri -> cols_indexes |> List.map (f ri))
  |> List.flatten

let transpose m =
  let rows, cols = get_dimensions m in
  let generate_transposed_row ci =
    List.init rows Fun.id |> List.map (fun ri -> element_at (ri, ci) m)
  in
  List.init cols Fun.id |> List.map generate_transposed_row

type diagonal_kind = Left | Right

let get_diagonals m =
  let rows, cols = get_dimensions m in
  let get_diagonal ri_start ci_start kind m =
    let col_exceeded_limit ci =
      match kind with Left -> ci >= cols | Right -> ci < 0
    in
    let r_step = -1 in
    let c_step = match kind with Left -> 1 | Right -> -1 in
    let rec inner m acc ri ci =
      if ri < 0 || col_exceeded_limit ci then acc |> List.rev
      else
        let ri_next = ri + r_step in
        let ci_next = ci + c_step in
        (inner [@tailcall]) m (element_at (ri, ci) m :: acc) ri_next ci_next
    in
    inner m [] ri_start ci_start
  in
  let get_left_diagonal (ri, ci) = get_diagonal ri ci Left m in
  let get_right_diagonal (ri, ci) = get_diagonal ri ci Right m in
  let left =
    List.init (rows - 1) Fun.id
    |> List.map (fun ri -> (ri, 0))
    |> List.map get_left_diagonal
  in
  let right =
    List.init (rows - 1) Fun.id
    |> List.map (fun ri -> (ri, cols - 1))
    |> List.map get_right_diagonal
  in
  let bottom_positions =
    List.init cols Fun.id |> List.map (fun ci -> (rows - 1, ci))
  in
  let bottom_left = bottom_positions |> List.map get_left_diagonal in
  let bottom_right = bottom_positions |> List.map get_right_diagonal in
  left @ right @ bottom_left @ bottom_right

let is_within_bounds m =
  let dimensions = get_dimensions m in
  let rows, cols = dimensions in
  fun (ri, ci) -> ri >= 0 && ri < rows && ci >= 0 && ci < cols
