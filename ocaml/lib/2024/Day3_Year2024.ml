let parse_puzzle_input raw_input ~remove_donts =
  let get_mul_numbers line =
    Re.Posix.compile_pat {|mul\(([0-9]{1,3}),([0-9]{1,3})\)|}
    |> Fun.flip Re.all line
    |> List.map (fun m -> (Re.Group.get m 1, Re.Group.get m 2))
  in
  let remove_donts_if_requested =
    if remove_donts then
      let mid_sections_re = Re.Perl.compile_pat {|don't\(\).*?do\(\)|} in
      let last_section_re = Re.Perl.compile_pat {|don't\(\).*|} in
      fun line ->
        Re.replace mid_sections_re line ~f:(fun _ -> "")
        |> Re.replace last_section_re ~f:(fun _ -> "")
    else Fun.id
  in
  raw_input
  |> String.concat "!"
  |> remove_donts_if_requested
  |> get_mul_numbers
  |> List.map (fun (a, b) -> (int_of_string a, int_of_string b))

let solution raw_input ~remove_donts =
  let puzzle_input = parse_puzzle_input raw_input ~remove_donts in
  let sum_of_muls =
    List.fold_left (fun acc v -> acc + (fst v * snd v)) 0 puzzle_input
  in
  Ok (string_of_int sum_of_muls)

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = solution ~remove_donts:false;
      day = 3;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "189600467";
    };
    {
      solution_fn = solution ~remove_donts:true;
      day = 3;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "107069718";
    };
  ]
