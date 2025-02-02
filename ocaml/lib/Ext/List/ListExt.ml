let count f ls = List.fold_left (fun acc v -> if f v then acc + 1 else acc) 0 ls

let count_from_index f idx ls =
  let rec inner f idx ls acc =
    match ls with
    | [] -> acc
    | x :: xs -> (
        match idx = 0 with
        | false -> (inner [@tailcall]) f (idx - 1) xs acc
        | true -> (inner [@tailcall]) f 0 xs (if f x then acc + 1 else acc))
  in
  inner f idx ls 0

let find_all_index f ls =
  let rec inner f ls acc idx =
    match ls with
    | [] -> List.rev acc
    | x :: xs -> (
        match f x with
        | false -> (inner [@tailcall]) f xs acc (idx + 1)
        | true -> (inner [@tailcall]) f xs (idx :: acc) (idx + 1))
  in
  inner f ls [] 0

let rev_index ls idx = List.length ls - 1 - idx

let find_nth_index f n ls =
  let rec inner f n ls idx =
    match ls with
    | [] -> None
    | x :: xs -> (
        match f x with
        | true ->
            if n = 1 then Some idx
            else (inner [@tailcall]) f (n - 1) xs (idx + 1)
        | false -> (inner [@tailcall]) f n xs (idx + 1))
  in
  if n <= 0 then None else inner f n ls 0

let find_nth_index_rev f n ls =
  find_nth_index f n ls |> Option.map (rev_index ls)

let find_index_rev f ls = List.find_index f ls |> Option.map (rev_index ls)

let find_index_wi f ls =
  let rec inner f ls idx =
    match ls with
    | [] -> None
    | x :: xs -> if f x idx then Some idx else (inner [@tailcall]) f xs (idx + 1)
  in
  inner f ls 0

let find_wi f ls =
  match find_index_wi f ls with
  | None -> None
  | Some idx -> Some (List.nth ls idx)

let rfind_index f ls =
  let ls = List.rev ls in
  List.find_index f ls |> Option.map (rev_index ls)

let remove_duplicates f ls =
  let step acc x =
    match List.find_index (f x) acc with Some _ -> acc | None -> x :: acc
  in
  List.fold_left step [] ls

let contains_duplicates f ls =
  let rec inner ls acc =
    match ls with
    | [] -> false
    | x :: xs -> (
        match List.find_index (f x) acc with
        | Some _ -> true
        | None -> (inner [@tailcall]) xs (x :: acc))
  in
  inner ls []

let chunk amount ls =
  let chunk_size = List.length ls / amount in
  let rec inner ls chunk_size acc current_chunk_acc =
    match ls with
    | [] -> current_chunk_acc :: acc
    | x :: xs ->
        if List.length current_chunk_acc = chunk_size then
          (inner [@tailcall]) xs chunk_size (current_chunk_acc :: acc) [ x ]
        else (inner [@tailcall]) xs chunk_size acc (x :: current_chunk_acc)
  in
  inner ls chunk_size [] [] |> List.map List.rev |> List.rev

let without_item_at idx ls =
  let rec inner ls target_idx acc current_idx =
    match ls with
    | [] -> acc
    | x :: xs ->
        if current_idx = target_idx then
          (inner [@tailcall]) xs target_idx acc (current_idx + 1)
        else (inner [@tailcall]) xs target_idx (x :: acc) (current_idx + 1)
  in
  inner ls idx [] 0 |> List.rev

let take n ls =
  let rec inner n ls acc =
    match (n, ls) with
    | 0, _ -> acc |> List.rev
    | _, [] -> acc |> List.rev
    | _, x :: xs -> (inner [@tailcall]) (n - 1) xs (x :: acc)
  in
  if n <= 0 then [] else inner n ls []

let drop n ls =
  let rec inner n ls =
    match ls with
    | [] -> []
    | x :: xs -> if n = 0 then x :: xs else (inner [@tailcall]) (n - 1) xs
  in
  if n <= 0 then ls else inner n ls

let with_item_at idx v ls =
  if idx < 0 || idx >= List.length ls then invalid_arg "with_item_at.idx"
  else take idx ls @ [ v ] @ drop (idx + 1) ls

let max f ls =
  let rec inner ls acc =
    match ls with
    | [] -> acc
    | x :: xs -> (
        match acc with
        | None -> (inner [@tailcall]) xs (Some x)
        | Some acc ->
            let new_acc = if f x acc > 0 then Some x else Some acc in
            (inner [@tailcall]) xs new_acc)
  in
  inner ls None

let swap idx1 idx2 ls =
  let len = List.length ls in
  let idx1_invalid = idx1 < 0 || idx1 >= len in
  let idx2_invalid = idx2 < 0 || idx2 >= len in
  match (idx1_invalid, idx2_invalid) with
  | true, _ -> invalid_arg "swap.idx1"
  | _, true -> invalid_arg "swap.idx2"
  | false, false ->
      let v1 = List.nth ls idx1 in
      let v2 = List.nth ls idx2 in
      let rec inner idx1 idx2 ls acc current_idx =
        match ls with
        | [] -> List.rev acc
        | x :: xs ->
            let cur =
              if current_idx = idx1 then v2
              else if current_idx = idx2 then v1
              else x
            in
            (inner [@tailcall]) idx1 idx2 xs (cur :: acc) (current_idx + 1)
      in
      inner idx1 idx2 ls [] 0

let par f ls_f ls =
  let threads_amount =
    min (Domain.recommended_domain_count ()) (List.length ls)
  in
  let results =
    let chunked = chunk threads_amount ls in
    let compute_chunk f idx () = ls_f f (List.nth chunked idx) in
    List.init (List.length chunked) (fun idx ->
        Domain.spawn (compute_chunk f idx))
  in
  let step acc domain = Domain.join domain :: acc in
  List.fold_left step [] results |> List.flatten

let par_map f ls = par f List.map ls
let par_filter f ls = par f List.filter ls
