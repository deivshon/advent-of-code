let keys h = Hashtbl.fold (fun k _ acc -> k :: acc) h []
let values h = Hashtbl.fold (fun _ v acc -> v :: acc) h []
let collect h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []

let upsert h k v =
  match Hashtbl.find_opt h k with
  | None -> Hashtbl.add h k v
  | Some _ -> Hashtbl.replace h k v
