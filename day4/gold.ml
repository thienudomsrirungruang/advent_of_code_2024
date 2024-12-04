
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
  
let check_mas (c: char) (d: char): bool =
  (c = 'M' && d = 'S') || (c = 'S' && d = 'M')

let () =
  record_backtrace true;
  let g = read_all_lines () |> make_grid in
  let (n, m) = get_dims g in
  let count = ref 0 in
  for i = 1 to n - 2 do
    for j = 1 to m - 2 do
      if g.(i).(j) = 'A' && check_mas (g.(i-1).(j-1)) (g.(i+1).(j+1)) && check_mas (g.(i-1).(j+1)) (g.(i+1).(j-1)) then
        count := !count + 1
    done
  done;
  print_int !count;
  print_newline ()
