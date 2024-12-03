let tuple_map2 f (a, b) = (f a, f b)

let rec list_fold_left2 f acc (l1, l2) =
  match (l1, l2) with
  | [], [] -> acc
  | x :: xs, y :: ys -> (list_fold_left2 [@tailcall]) f (f acc x y) (xs, ys)
  | _ -> invalid_arg "lists must have the same length"

let uncurry3 f (a, b, c) = f a b c

let list_count f l =
  List.fold_left (fun acc v -> if f v then acc + 1 else acc) 0 l

let without_element_at idx l =
  let rec inner l target_idx acc current_idx =
    match l with
    | [] -> acc
    | x :: xs ->
        if current_idx = target_idx then
          (inner [@tailcall]) xs target_idx acc (current_idx + 1)
        else (inner [@tailcall]) xs target_idx (x :: acc) (current_idx + 1)
  in
  inner l idx [] 0 |> List.rev

let remove_empty_strings = List.filter (fun line -> String.length line > 0)
