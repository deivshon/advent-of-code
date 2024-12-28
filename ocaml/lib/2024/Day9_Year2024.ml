module SingleBlock = struct
  type t = File of int | Free

  let is_file block = match block with File _ -> true | _ -> false
  let is_free block = match block with Free -> true | _ -> false

  let get_id block =
    match block with File id -> id | _ -> invalid_arg "get_id.block"

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

  let disk_checksum ls =
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
end

module GroupedBlock = struct
  type file_info = { id : int; space : int }
  type t = File of file_info | Free of int

  let is_file ?(id = None) block =
    match block with
    | File info -> ( match id with Some id -> info.id = id | None -> true)
    | Free _ -> false

  let is_free ?(min_space = None) block =
    match block with
    | File _ -> false
    | Free space -> (
        match min_space with
        | Some min_space -> space >= min_space
        | None -> true)

  let get_file_info block =
    match block with
    | File info -> { id = info.id; space = info.space }
    | Free _ -> invalid_arg "get_info.block"

  let get_free_space block =
    match block with
    | File _ -> invalid_arg "get_free_amount.block"
    | Free amount -> amount

  let get_space block =
    match block with File info -> info.space | Free space -> space

  let parse_disk disk =
    let expand_block space id =
      match id with Some id -> File { id; space } | None -> Free space
    in
    let rec inner disk acc id is_free =
      match disk with
      | [] -> acc |> List.rev
      | x :: xs ->
          let cur = expand_block x (if is_free then None else Some id) in
          let new_id = if is_free then id else id + 1 in
          (inner [@tailcall]) xs (cur :: acc) new_id (not is_free)
    in
    inner disk [] 0 false

  let disk_checksum ls =
    let with_moved_file ls id =
      let file_idx = List.find_index (is_file ~id:(Some id)) ls |> Option.get in
      let file_info = List.nth ls file_idx |> get_file_info in
      let free_block_fits block idx =
        is_free ~min_space:(Some file_info.space) block && idx < file_idx
      in
      let fitting_free_block = ListExt.find_wi free_block_fits ls in
      match fitting_free_block with
      | None -> ls
      | Some block ->
          let fitting_free_block_idx =
            ListExt.find_index_wi free_block_fits ls |> Option.get
          in
          let decreased_free_space = get_free_space block - file_info.space in
          let updated_free_block = Free decreased_free_space in
          let needs_updated_free_block = decreased_free_space > 0 in
          let new_file_idx =
            if needs_updated_free_block then file_idx + 1 else file_idx
          in
          let moved_file = File file_info in
          ListExt.take fitting_free_block_idx ls
          @ [ moved_file ]
          @ (if needs_updated_free_block then [ updated_free_block ] else [])
          @ ListExt.drop (fitting_free_block_idx + 1) ls
          |> ListExt.with_item_at new_file_idx (Free file_info.space)
    in
    let file_max_id =
      ls
      |> List.filter is_file
      |> List.map get_file_info
      |> ListExt.max (fun x y -> x.id - y.id)
      |> Option.get
    in
    let modified =
      List.init (file_max_id.id + 1) Fun.id
      |> List.rev
      |> List.fold_left with_moved_file ls
    in
    let step acc block =
      let cur_sum, idx = acc in
      let space = get_space block in
      let compute_grouped_file id =
        List.init space (( + ) idx) |> List.map (( * ) id) |> IntList.sum
      in
      match block with
      | File info -> (cur_sum + compute_grouped_file info.id, idx + space)
      | Free _ -> (cur_sum, idx + space)
    in
    List.fold_left step (0, 0) modified |> fst
end

let parse_puzzle_input raw_input =
  raw_input
  |> StringList.remove_empty
  |> (Fun.flip List.nth) 0
  |> CharList.from_string
  |> List.map CharExt.to_string
  |> List.map int_of_string

let first_solution raw_input =
  parse_puzzle_input raw_input
  |> SingleBlock.parse_disk
  |> SingleBlock.disk_checksum
  |> string_of_int
  |> Result.ok

let second_solution raw_input =
  parse_puzzle_input raw_input
  |> GroupedBlock.parse_disk
  |> GroupedBlock.disk_checksum
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
    {
      solution_fn = second_solution;
      day = 9;
      year = 2024;
      solution_number = 2;
      expected_solution = Some "6493634986625";
    };
  ]
