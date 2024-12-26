type block = File of int | Free

let is_file block = match block with File _ -> true | _ -> false
let is_free block = match block with Free -> true | _ -> false

let get_id block =
  match block with File id -> id | _ -> invalid_arg "get_id.block"

let parse_puzzle_input raw_input =
  let parse_disk disk =
    let expand_block n id =
      let init_single _ = match id with Some id -> File id | None -> Free in
      List.init n init_single
    in
    let rec inner disk acc id is_free =
      match disk with
      | [] -> acc |> List.rev |> List.flatten
      | x :: xs ->
          let current = expand_block x (if is_free then None else Some id) in
          let new_id = if is_free then id + 1 else id in
          (inner [@tailcall]) xs (current :: acc) new_id (not is_free)
    in
    inner disk [] 0 false
  in
  raw_input
  |> StringList.remove_empty
  |> (Fun.flip List.nth) 0
  |> CharList.from_string
  |> List.map CharExt.to_string
  |> List.map int_of_string
  |> parse_disk

let checksum ls =
  let rev = List.rev ls in
  let r_file_idx_start = ListExt.find_index_rev is_file rev |> Option.get in
  let rec inner r_file_idx acc nth idx =
    let cur = List.nth ls idx in
    match cur with
    | Free ->
        if idx > r_file_idx then acc
        else
          let current_id = get_id (List.nth ls r_file_idx) in
          let new_r_file_idx =
            ListExt.find_nth_index_rev is_file nth rev |> Option.get
          in
          (inner [@tailcall]) new_r_file_idx
            (acc + (current_id * idx))
            (nth + 1) (idx + 1)
    | File id ->
        let remaining = ListExt.count_from_index is_file idx ls in
        let new_acc = acc + (id * idx) in
        if remaining < nth then new_acc
        else (inner [@tailcall]) r_file_idx new_acc nth (idx + 1)
  in
  inner r_file_idx_start 0 2 0

let first_solution raw_input =
  parse_puzzle_input raw_input |> checksum |> string_of_int |> Result.ok

let (solutions : Execution.solution list) =
  [
    {
      solution_fn = first_solution;
      day = 9;
      year = 2024;
      solution_number = 1;
      expected_solution = Some "6463499258318";
    };
  ]
