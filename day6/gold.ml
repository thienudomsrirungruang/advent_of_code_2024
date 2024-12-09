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

let dirs = [|(-1, 0); (0, 1); (1, 0); (0, -1)|]

let find_start (grid: char array array): (int * int) =
  let n = Array.length grid in
  let m = Array.length grid.(0) in
  let pos = ref (-1, -1) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if grid.(i).(j) = '^' then
        pos := (i, j)
    done
  done;
  if !pos = (-1, -1) then
    failwith "No start found"
  else
    !pos

let is_stuck (inputs: char array array): bool =
  let n = Array.length inputs in
  let m = Array.length inputs.(0) in
  let inbounds (i: int) (j: int): bool = i >= 0 && i < n && j >= 0 && j < m in
  let vis = Array.init 4 (fun _ -> Array.make_matrix n m false) in
  let (sx, sy) = find_start inputs in
  let rec visit (i: int) (j: int) (dir: int) =
    if not (inbounds i j) then
      false
    else if vis.(dir).(i).(j) then
      true
    else
      begin
        vis.(dir).(i).(j) <- true;
        let a = i + fst dirs.(dir) in
        let b = j + snd dirs.(dir) in
        if inbounds a b && inputs.(a).(b) = '#' then
          visit i j ((dir + 1) mod 4)
        else
          visit a b dir 
      end
  in
  visit sx sy 0
  

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> turn_into_grid in
  let n = Array.length inputs in
  let m = Array.length inputs.(0) in
  let res = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if inputs.(i).(j) = '.' then
      begin
        inputs.(i).(j) <- '#';
        if is_stuck inputs then
          incr res;
        inputs.(i).(j) <- '.';
      end
    done
  done;
  Printf.printf "%d\n" !res
