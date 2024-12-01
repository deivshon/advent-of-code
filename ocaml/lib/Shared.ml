let tuple_map2 f (a, b) = (f a, f b)

let rec list_fold_left2 f acc (l1, l2) =
  match (l1, l2) with
  | [], [] -> acc
  | x :: xs, y :: ys -> (list_fold_left2 [@tailcall]) f (f acc x y) (xs, ys)
  | _ -> invalid_arg "lists must have the same length"

let uncurry3 f (a, b, c) = f a b c

let list_count f l =
  List.fold_left (fun acc v -> if f v then acc + 1 else acc) 0 l

let remove_empty_lines = List.filter (fun line -> String.length line > 0)
