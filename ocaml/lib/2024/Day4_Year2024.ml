let parse_puzzle_input raw_input =
  raw_input |> StringList.remove_empty |> List.map CharList.from_string

let char_matrix_to_lines matrix = matrix |> List.map CharList.to_string

let count_xmas s =
  let xmas_re = Re.Posix.compile_pat {|XMAS|} in
  let samx_re = Re.Posix.compile_pat {|SAMX|} in
  List.length (Re.all xmas_re s) + List.length (Re.all samx_re s)

let first_solution raw_input =
  let map = parse_puzzle_input raw_input in
  let standard = map |> char_matrix_to_lines in
  let transposed = Matrix.transpose map |> char_matrix_to_lines in
  let diagonals = Matrix.get_diagonals map |> char_matrix_to_lines in
  let count =
    List.map count_xmas (standard @ transposed @ diagonals) |> IntList.sum
  in
  Ok (string_of_int count)

let count_x_mas map =
  let rows, cols = Matrix.get_dimensions map in
  let all_corners_exist ri ci =
    (ri >= 1 && ri <= rows - 2) && ci >= 1 && ci <= cols - 2
  in
  let has_mas_corners ri ci =
    if not (all_corners_exist ri ci) then false
    else
      let top_left = Matrix.element_at (ri - 1, ci - 1) map in
      let top_right = Matrix.element_at (ri - 1, ci + 1) map in
      let bottom_left = Matrix.element_at (ri + 1, ci - 1) map in
      let bottom_right = Matrix.element_at (ri + 1, ci + 1) map in
      ((top_left = 'M' && bottom_right = 'S')
      || (top_left = 'S' && bottom_right = 'M'))
      && ((top_right = 'M' && bottom_left = 'S')
         || (top_right = 'S' && bottom_left = 'M'))
  in
  let is_x_mas_center ri ci =
    let center = Matrix.element_at (ri, ci) map in
    center = 'A' && has_mas_corners ri ci
  in
  Matrix.init_flatten is_x_mas_center map |> BoolList.count_true

let second_solution raw_input =
  let map = parse_puzzle_input raw_input in
  Ok (count_x_mas map) |> Result.map string_of_int

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
