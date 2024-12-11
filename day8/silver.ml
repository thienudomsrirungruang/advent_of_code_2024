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

let turn_into_grid (xs: string list): char array array =
  let n = List.length xs in
  let m = String.length (List.hd xs) in
  let grid = Array.make_matrix n m ' ' in
  List.iteri (fun i x ->
    String.iteri (fun j c ->
      grid.(i).(j) <- c
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

let inbounds (grid: char array array) (pos: Pos.t): bool =
  let n = Array.length grid in
  let m = Array.length grid.(0) in
  0 <= pos.x && pos.x < n && 0 <= pos.y && pos.y < m

let get (grid: char array array) (pos: Pos.t): char =
  if not (inbounds grid pos) then
    raise (Invalid_argument "Out of bounds")
  else
  grid.(pos.x).(pos.y)

let positions (n: int) (m: int): Pos.t list =
  List.init n (fun x -> List.init m (fun y -> Pos.make x y)) |> List.flatten

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> turn_into_grid in
  let n = Array.length inputs in
  let m = Array.length inputs.(0) in
  let all_squares = inputs |> Array.to_list |> List.map (Array.to_list) |> List.flatten in
  let unique = List.sort_uniq Char.compare all_squares |> List.filter (fun x -> x <> '.') in
  let good = Array.make_matrix n m false in
  List.iter (fun c -> 
    let ant = positions n m |> List.filter (fun pos -> get inputs pos = c) in
    List.iter (fun p1 ->
      List.iter (fun p2 ->
        if p1 <> p2 then
          begin
            let a = Pos.sub (Pos.mul p2 2) p1 in
            let b = Pos.sub (Pos.mul p1 2) p2 in
            if inbounds inputs a then good.(a.x).(a.y) <- true;
            if inbounds inputs b then good.(b.x).(b.y) <- true;
          end 
      ) ant
    ) ant
  ) unique;
  let res = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if good.(i).(j) then
        res := !res + 1
    done
  done;
  print_int !res;
  print_newline ()
