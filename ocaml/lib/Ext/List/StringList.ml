let remove_empty = List.filter (fun line -> String.length line > 0)

let rstrip_empty l =
  let rec inner l acc keep_removing =
    match l with
    | [] -> acc
    | x :: xs ->
        if not keep_removing then
          (inner [@tailcall]) xs (x :: acc) keep_removing
        else if String.length x = 0 then
          (inner [@tailcall]) xs acc keep_removing
        else (inner [@tailcall]) xs (x :: acc) false
  in
  inner (List.rev l) [] true
