let digits_only str =
  CharList.from_string str |> List.filter CharExt.is_digit |> CharList.to_string

let is_empty str = String.length str = 0
let not_empty str = not (is_empty str)
