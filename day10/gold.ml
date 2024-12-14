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

let turn_into_grid (xs: string list): int array array =
  let n = List.length xs in
  let m = String.length (List.hd xs) in
  let grid = Array.make_matrix n m 0 in
  List.iteri (fun i x ->
    String.iteri (fun j c ->
      grid.(i).(j) <- int_of_string (String.make 1 c)
    ) x
  ) xs;
  grid

module Pos = struct
  type t = { x: int; y: int }
  
  let make (x: int) (y: int): t = { x; y }
  let add (p: t) (q: t): t = { x = p.x + q.x; y = p.y + q.y }
  let sub (p: t) (q: t): t = { x = p.x - q.x; y = p.y - q.y }
  let mul (p: t) (k: int): t = { x = p.x * k; y = p.y * k }
end

let inbounds (grid: 'a array array) (pos: Pos.t): bool =
  let n = Array.length grid in
  let m = Array.length grid.(0) in
  0 <= pos.x && pos.x < n && 0 <= pos.y && pos.y < m

let get (grid: 'a array array) (pos: Pos.t): 'a =
  if not (inbounds grid pos) then
    raise (Invalid_argument "Out of bounds")
  else
  grid.(pos.x).(pos.y)

let positions (n: int) (m: int): Pos.t list =
  List.init n (fun x -> List.init m (fun y -> Pos.make x y)) |> List.flatten

let dirs = [
  Pos.make 1 0;
  Pos.make 0 1;
  Pos.make (-1) 0;
  Pos.make 0 (-1)
]

module PosSet = Set.Make(
  struct
    type t = Pos.t
    let compare = compare
  end
)

let rec reachable (grid: int array array) (pos: Pos.t): int =
  let current = grid.(pos.x).(pos.y) in
  if current = 9 then
    1
  else
    let proc_arm dir =
      let next = Pos.add pos dir in
      if inbounds grid next && grid.(next.x).(next.y) = current + 1 then
        reachable grid next
      else
        0
    in
    List.map proc_arm dirs |> List.fold_left (+) 0

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> turn_into_grid in
  let n = Array.length inputs in
  let m = Array.length inputs.(0) in
  let ans = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if inputs.(i).(j) = 0 then
        ans := !ans + (reachable inputs (Pos.make i j))
    done
  done;
  print_int !ans;
  print_newline ()
