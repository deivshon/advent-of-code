let remove_empty = List.filter (fun line -> String.length line > 0)

let rstrip_empty ls =
  let rec inner ls acc keep_removing =
    match ls with
    | [] -> acc
    | x :: xs ->
        if not keep_removing then
          (inner [@tailcall]) xs (x :: acc) keep_removing
        else if String.length x = 0 then
          (inner [@tailcall]) xs acc keep_removing
        else (inner [@tailcall]) xs (x :: acc) false
  in
  inner (List.rev ls) [] true
