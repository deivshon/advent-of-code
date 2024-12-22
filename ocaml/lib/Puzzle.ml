open Core

let get_input ~year ~day =
  ("../puzzle-inputs/%s/%s.txt", string_of_int year, string_of_int day)
  |> FunExt.uncurry3 Printf.sprintf
  |> In_channel.read_lines
