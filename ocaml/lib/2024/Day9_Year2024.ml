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
  let file_blocks = List.filter is_file ls |> List.map get_id in
  List.init (List.length file_blocks) Fun.id
  |>
  let step acc idx = acc + (idx * List.nth file_blocks idx) in
  List.fold_left step 0

let reorder_blocks ls =
  let rec inner acc l_free_idx r_file_idx =
    if l_free_idx > r_file_idx then acc
    else
      let new_acc = ListExt.swap l_free_idx r_file_idx acc in
      let new_l_free_idx = List.find_index is_free new_acc |> Option.get in
      let new_r_file_idx = ListExt.rfind_index is_file new_acc |> Option.get in
      (inner [@tailcall]) new_acc new_l_free_idx new_r_file_idx
  in
  inner ls (List.find_index is_free ls |> Option.get) (List.length ls - 1)

let first_solution raw_input =
  parse_puzzle_input raw_input
  |> reorder_blocks
  |> checksum
  |> string_of_int
  |> Result.ok

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
