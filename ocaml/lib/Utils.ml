open Core
open Shared

let get_puzzle_input_lines ~year ~day =
  ("../puzzle-inputs/%s/%s.txt", string_of_int year, string_of_int day)
  |> uncurry3 Printf.sprintf
  |> In_channel.read_lines
