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

type problem = {
  target: int;
  nums: int list;
}

let parse (line: string): problem =
  let t :: ns :: [] = String.split_on_char ':' line in
  let target = int_of_string t in
  let nums = List.map int_of_string (String.trim ns |> String.split_on_char ' ') in
  {target = target; nums = nums}

type op = Plus | Times | Concat

let rec opmasks (b: int) : op list list =
  if b = 0 then [[]]
  else let rest = opmasks (b-1) in
    List.concat (List.map (fun o -> List.map (fun x -> o :: x) rest) [Plus; Times; Concat])

(* let opmasks (b: int): op list list =
  let make (msk: int): bool list =
    List.init b (fun i -> (msk lsr i) land 1 = 1)
  in
  List.init (1 lsl b) make *)

let apply (op: op) (x: int) (y: int): int =
  match op with
  | Plus -> x + y
  | Times -> x * y
  | Concat -> int_of_string (string_of_int x ^ string_of_int y)

let solve (p: problem): bool =
  let n = List.length p.nums in
  let masks = opmasks (n-1) in
  let eval (msk: op list): int =
    List.fold_left (fun acc (op, x) -> apply op acc x) (List.hd p.nums) (List.tl p.nums |> List.combine msk)
  in
  List.exists (fun msk -> eval msk = p.target) masks

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> List.map parse in
  let res = List.filter solve inputs in
  print_int (res |> List.map (fun p -> p.target) |> List.fold_left (+) 0);
  print_newline ()

