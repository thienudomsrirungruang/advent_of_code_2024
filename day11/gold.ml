open Printexc
open Hashtbl

let memoizer = create 1000

let rec length (x: int) (t: int): int =
  if Hashtbl.find_opt memoizer (x, t) |> Option.is_some then
    Hashtbl.find memoizer (x, t)
  else
    let result = length' x t in
    Hashtbl.replace memoizer (x, t) result;
    result
and length' (x: int) (t: int): int =
  if t = 0 then 1
  else if x = 0 then length 1 (t-1)
  else
    let s = string_of_int x in
    if (String.length s) mod 2 = 0 then
      let half = (String.length s) / 2 in
      let a = String.sub s 0 half in
      let b = String.sub s half half in
      length (int_of_string a) (t-1) + length (int_of_string b) (t-1)
    else
      length (x * 2024) (t-1) 

let time = 75

let () =
  record_backtrace true;
  let input = read_line () |> String.split_on_char ' ' |> List.map int_of_string in
  let output = List.fold_left (+) 0 (List.map (fun x -> length x time) input) in
  print_int output;
  print_newline ()
