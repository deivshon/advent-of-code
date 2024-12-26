let is_digit chr =
  let code = Char.code chr in
  code >= 48 && code <= 57

let to_string = Printf.sprintf "%c"
