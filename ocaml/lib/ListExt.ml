let chunk_list chunks_amount ls =
  let chunk_size = List.length ls / chunks_amount in
  let rec inner ls chunk_size acc current_chunk_acc =
    match ls with
    | [] -> current_chunk_acc :: acc
    | x :: xs ->
        if List.length current_chunk_acc = chunk_size then
          (inner [@tailcall]) xs chunk_size (current_chunk_acc :: acc) [ x ]
        else (inner [@tailcall]) xs chunk_size acc (x :: current_chunk_acc)
  in
  inner ls chunk_size [] [] |> List.map List.rev |> List.rev

let par_map f ls =
  let threads_amount =
    min (Domain.recommended_domain_count ()) (List.length ls)
  in
  let chunked = chunk_list threads_amount ls in
  let map_chunk f idx () = List.map f (List.nth chunked idx) in
  let results =
    List.init (List.length chunked) (fun idx -> Domain.spawn (map_chunk f idx))
  in
  let step acc domain = Domain.join domain :: acc in
  List.fold_left step [] results |> List.flatten
