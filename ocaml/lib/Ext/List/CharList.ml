let to_string ls = String.of_seq (List.to_seq ls)
let from_string str = List.init (String.length str) (String.get str)
