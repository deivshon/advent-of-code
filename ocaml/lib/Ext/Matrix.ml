let element_at ~row ~col matrix = List.nth (List.nth matrix row) col

exception Invalid_matrix of string

let get_dimensions matrix =
  let rows = List.length matrix in
  let get_columns_number matrix =
    let rec inner matrix column_number =
      match matrix with
      | [] -> ( match column_number with Some v -> Ok v | None -> Ok 0)
      | x :: xs -> (
          let current_row_columns = List.length x in
          match column_number with
          | Some c ->
              if c != current_row_columns then
                Error "rows don't all have the same amount of columns"
              else (inner [@tailcall]) xs column_number
          | None -> (inner [@tailcall]) xs (Some current_row_columns))
    in
    inner matrix None
  in
  let columns = get_columns_number matrix in
  match columns with
  | Ok columns -> (rows, columns)
  | Error _ ->
      raise
        (Invalid_matrix "rows number did not always match with columns number")

let find_index f matrix =
  let rec inner f matrix row_idx =
    match matrix with
    | [] -> raise Not_found
    | x :: xs -> (
        match List.find_index f x with
        | Some matching_col_idx -> (row_idx, matching_col_idx)
        | None -> (inner [@tailcall]) f xs (row_idx + 1))
  in
  inner f matrix 0

let find_all_index f matrix =
  let rec inner f matrix row_idx acc =
    match matrix with
    | [] -> acc |> List.rev |> List.flatten
    | x :: xs ->
        let found_in_row =
          ListExt.find_all_index f x
          |> List.map (fun col_idx -> (row_idx, col_idx))
        in
        (inner [@tailcall]) f xs (row_idx + 1) (found_in_row :: acc)
  in
  inner f matrix 0 []

let map f matrix = List.map (fun row -> List.map f row) matrix

let init_flatten f matrix =
  let rows, cols = get_dimensions matrix in
  let rows_indexes = List.init rows Fun.id in
  let cols_indexes = List.init cols Fun.id in
  rows_indexes
  |> List.map (fun row_idx ->
         cols_indexes |> List.map (fun col_idx -> f row_idx col_idx))
  |> List.flatten

let transpose matrix =
  let rows, columns = get_dimensions matrix in
  let generate_transposed_row col_idx =
    List.init rows Fun.id
    |> List.map (fun row_idx -> element_at ~row:row_idx ~col:col_idx matrix)
  in
  List.init columns Fun.id |> List.map generate_transposed_row

type diagonal_kind = Left | Right

let get_diagonals matrix =
  let rows, columns = get_dimensions matrix in
  let get_diagonal row_start_idx col_start_idx kind matrix =
    let col_exceeded_limit col_idx =
      match kind with Left -> col_idx >= columns | Right -> col_idx < 0
    in
    let col_step = match kind with Left -> 1 | Right -> -1 in
    let rec inner matrix acc row_idx col_idx =
      if row_idx < 0 || col_exceeded_limit col_idx then acc
      else
        let row_idx_next = row_idx - 1 in
        let col_idx_next = col_idx + col_step in
        (inner [@tailcall]) matrix
          (element_at ~row:row_idx ~col:col_idx matrix :: acc)
          row_idx_next col_idx_next
    in
    inner matrix [] row_start_idx col_start_idx |> List.rev
  in
  let get_left_diagonal (row_idx, col_idx) =
    get_diagonal row_idx col_idx Left matrix
  in
  let get_right_diagonal (row_idx, col_idx) =
    get_diagonal row_idx col_idx Right matrix
  in
  let left =
    List.init (rows - 1) Fun.id
    |> List.map (fun row_idx -> (row_idx, 0))
    |> List.map get_left_diagonal
  in
  let right =
    List.init (rows - 1) Fun.id
    |> List.map (fun row_idx -> (row_idx, columns - 1))
    |> List.map get_right_diagonal
  in
  let bottom_positions =
    List.init columns Fun.id |> List.map (fun col_idx -> (rows - 1, col_idx))
  in
  let bottom_left = bottom_positions |> List.map get_left_diagonal in
  let bottom_right = bottom_positions |> List.map get_right_diagonal in
  left @ right @ bottom_left @ bottom_right

let is_within_bounds matrix =
  let dimensions = get_dimensions matrix in
  let rows, cols = dimensions in
  fun ~row ~col -> row >= 0 && row < rows && col >= 0 && col < cols
