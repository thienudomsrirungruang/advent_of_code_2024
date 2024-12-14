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

let dirs = [Pos.make 1 0; Pos.make 0 1; Pos.make (-1) 0; Pos.make 0 (-1)]

let process_region (grid: char array array) (start: Pos.t) (vis: bool array array) =
  let area = ref 0 in
  let perimeter = ref 0 in
  let rec dfs (p: Pos.t) =
    area := !area + 1;
    vis.(p.x).(p.y) <- true;
    List.init 4 (fun x -> x) |> List.iter (fun i ->
      let d = List.nth dirs i in
      let p' = Pos.add p d in
      if inbounds grid p' && not vis.(p'.x).(p'.y) && get grid p' = get grid start then
        dfs p';
      if not (inbounds grid p') || not (get grid p' = get grid start) then
        (* check if this is a continuation of the same side? *)
        let dir_before = List.nth dirs ((i+1) mod 4) in
        let in_before = Pos.add p dir_before in
        let out_before = Pos.add in_before d in
        if inbounds grid in_before && get grid in_before = get grid start && (not (inbounds grid out_before) || get grid out_before <> get grid start) then
          ()
        else
          perimeter := !perimeter + 1;
    )
  in
  dfs start;
  print_string (String.make 1 (get grid start));
  print_string " ";
  print_int !area;
  print_string " ";
  print_int !perimeter;
  print_newline ();
  !area * !perimeter

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> turn_into_grid in
  let n = Array.length inputs in
  let m = Array.length inputs.(0) in
  let vis = Array.make_matrix n m false in
  let ans = ref 0 in
  positions n m |> List.iter (fun (pos: Pos.t) ->
    if not (vis.(pos.x).(pos.y)) then
      begin
        let res = process_region inputs pos vis in
        ans := !ans + res;
      end
  );
  print_int !ans;
  print_newline ()

