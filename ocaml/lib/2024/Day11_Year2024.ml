let parse_puzzle_input raw_input =
  raw_input
  |> StringList.remove_empty
  |> (Fun.flip List.nth) 0
  |> String.split_on_char ' '
  |> List.map int_of_string

let digits_amount n = String.length (string_of_int n)

let middle_split n =
  let n_char_list = string_of_int n |> CharList.from_string in
  let len = List.length n_char_list in
  if IntExt.is_odd len then invalid_arg "middle_split.n"
  else
    [ ListExt.take (len / 2) n_char_list; ListExt.drop (len / 2) n_char_list ]
    |> List.map CharList.to_string
    |> List.map int_of_string

let stone_length_after iterations line =
  let table = Hashtbl.create 16184 in
  let _mut_insert_first =
    line |> List.iter (fun n -> Hashtbl.add table n (1, 0))
  in
  let mut_stone_step h n amount =
    let mut_add_to_buffer h n amount =
      let cur = Hashtbl.find_opt h n in
      match cur with
      | None -> Hashtbl.add h n (0, amount)
      | Some (prev_amount, buffer) ->
          Hashtbl.replace h n (prev_amount, buffer + amount)
    in
    let _mut_remove_current = mut_add_to_buffer h n (-amount) in
    (match (n, IntExt.is_even (digits_amount n)) with
    | 0, _ -> [ 1 ]
    | _, true -> middle_split n
    | _, false -> [ n * 2024 ])
    |> List.iter (fun new_n -> mut_add_to_buffer h new_n amount)
  in
  let mut_finalize_line_step h =
    let mut_finalize_single h k =
      let prev_amount, buffer = Hashtbl.find h k in
      Hashtbl.replace h k (prev_amount + buffer, 0)
    in
    HashtblExt.keys h |> List.iter (mut_finalize_single h)
  in
  let rec inner h iterations =
    match iterations with
    | 0 -> Hashtbl.fold (fun _ (amount, _) acc -> acc + amount) table 0
    | _ ->
        let _mut_line_step =
          HashtblExt.collect h
          |> List.iter (fun (n, (amount, _)) -> mut_stone_step h n amount)
        in
        let _mut_finalize = mut_finalize_line_step h in
        (inner [@tailcall]) h (iterations - 1)
  in
  inner table iterations

let solution ~iterations raw_input =
  raw_input
  |> parse_puzzle_input
  |> stone_length_after iterations
  |> string_of_int
  |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = solution ~iterations:25;
      day = 11;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "233875";
    };
    {
      solution_fn = solution ~iterations:75;
      day = 11;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "277444936413293";
    };
  ]
