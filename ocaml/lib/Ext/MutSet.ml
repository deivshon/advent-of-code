let create initial_size = Hashtbl.create initial_size
let add s v = Hashtbl.add s v ()
let mem s v = Hashtbl.mem s v
let not_mem s v = Hashtbl.mem s v |> Bool.not
let items s = HashtblExt.keys s
