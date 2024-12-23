let count f ls = List.fold_left (fun acc v -> if f v then acc + 1 else acc) 0 ls

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

let without_element_at idx ls =
  let rec inner ls target_idx acc current_idx =
    match ls with
    | [] -> acc
    | x :: xs ->
        if current_idx = target_idx then
          (inner [@tailcall]) xs target_idx acc (current_idx + 1)
        else (inner [@tailcall]) xs target_idx (x :: acc) (current_idx + 1)
  in
  inner ls idx [] 0 |> List.rev

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
