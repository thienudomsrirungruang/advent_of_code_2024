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

let bitmasks (b: int): bool list list =
  let make (msk: int): bool list =
    List.init b (fun i -> (msk lsr i) land 1 = 1)
  in
  List.init (1 lsl b) make

let solve (p: problem): bool =
  let n = List.length p.nums in
  let masks = bitmasks (n-1) in
  let eval (msk: bool list): int =
    List.fold_left (fun acc (op, x) -> if op then acc + x else acc * x) (List.hd p.nums) (List.tl p.nums |> List.combine msk)
  in
  List.exists (fun msk -> eval msk = p.target) masks

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> List.map parse in
  let res = List.filter solve inputs in
  print_int (res |> List.map (fun p -> p.target) |> List.fold_left (+) 0);
  print_newline ()

