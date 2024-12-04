
open Printexc

let read_all_lines (): string list =
  let rec f () =
    try
      let line = read_line () in
      line :: f ()
    with
    | End_of_file -> []
  in
  f ()

let make_grid (xs: string list): char array array =
  let make_row (s: string): char array = Array.init (String.length s) (fun i -> String.get s i) in
  Array.of_list (List.map make_row xs)

let get_dims (g: 'a array array): int * int =
  (Array.length g, Array.length g.(0))

let dirs = List.filter (fun (x, y) -> x <> 0 || y <> 0) (List.init 9 (fun i -> (i mod 3 - 1, i / 3 - 1)))

let search_string = "XMAS"
let search_len = String.length search_string

let () =
  record_backtrace true;
  let g = read_all_lines () |> make_grid in
  let (n, m) = get_dims g in
  let inbounds (x, y) = 0 <= x && x < n && 0 <= y && y < m in
  let count = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      List.iter (fun (dx, dy) ->
        let locations = List.init search_len (fun k -> (i + k * dx, j + k * dy)) in
        if List.for_all inbounds locations then
          let letters = List.map (fun (x, y) -> g.(x).(y)) locations in
          let pairs = List.combine letters (search_string |> String.to_seq |> List.of_seq) in
          if List.for_all (fun (x, y) -> x = y) pairs then
            count := !count + 1
      ) dirs
    done
  done;
  print_int !count;
  print_newline ()
