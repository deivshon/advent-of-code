let parse_puzzle_input raw_input =
  raw_input |> StringList.remove_empty |> List.map CharList.from_string

let char_matrix_to_lines matrix = matrix |> List.map CharList.to_string

let count_xmas s =
  let xmas_re = Re.Posix.compile_pat {|XMAS|} in
  let samx_re = Re.Posix.compile_pat {|SAMX|} in
  List.length (Re.all xmas_re s) + List.length (Re.all samx_re s)

let first_solution raw_input =
  let char_matrix = parse_puzzle_input raw_input in
  let standard = char_matrix |> char_matrix_to_lines in
  let transposed = Matrix.transpose char_matrix |> char_matrix_to_lines in
  let diagonals = Matrix.get_diagonals char_matrix |> char_matrix_to_lines in
  let count =
    List.map count_xmas (standard @ transposed @ diagonals)
    |> List.fold_left ( + ) 0
  in
  Ok (string_of_int count)

let count_x_mas matrix =
  let rows, cols = Matrix.get_dimensions matrix in
  let all_corners_exist row_idx col_idx =
    (row_idx >= 1 && row_idx <= rows - 2) && col_idx >= 1 && col_idx <= cols - 2
  in
  let has_mas_corners row_idx col_idx =
    if not (all_corners_exist row_idx col_idx) then false
    else
      let top_left =
        Matrix.element_at ~row:(row_idx - 1) ~col:(col_idx - 1) matrix
      in
      let top_right =
        Matrix.element_at ~row:(row_idx - 1) ~col:(col_idx + 1) matrix
      in
      let bottom_left =
        Matrix.element_at ~row:(row_idx + 1) ~col:(col_idx - 1) matrix
      in
      let bottom_right =
        Matrix.element_at ~row:(row_idx + 1) ~col:(col_idx + 1) matrix
      in
      ((top_left = 'M' && bottom_right = 'S')
      || (top_left = 'S' && bottom_right = 'M'))
      && ((top_right = 'M' && bottom_left = 'S')
         || (top_right = 'S' && bottom_left = 'M'))
  in
  let is_x_mas_center row_idx col_idx =
    let center = Matrix.element_at ~row:row_idx ~col:col_idx matrix in
    center = 'A' && has_mas_corners row_idx col_idx
  in
  Matrix.init_flatten is_x_mas_center matrix |> ListExt.count Fun.id

let second_solution raw_input =
  let char_matrix = parse_puzzle_input raw_input in
  Ok (count_x_mas char_matrix) |> Result.map string_of_int

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 4;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "2504";
    };
    {
      solution_fn = second_solution;
      day = 4;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "1923";
    };
  ]
