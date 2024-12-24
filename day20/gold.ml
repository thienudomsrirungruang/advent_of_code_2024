open Printexc

#use "../util/vec2.ml"

let (>>) f g x = g (f x)

let read_grid () : char array array =
  let rec f () =
    try
      let line = read_line () in
      line :: f ()
    with End_of_file -> []
  in
  f () |> List.map (String.to_seq >> Array.of_seq) |> Array.of_list

let map2d = Array.map >> Array.map

let find (grid: 'a array array) (p: 'a -> bool): Vec2.t option =
  let (n, m) = (Array.length grid, Array.length grid.(0)) in
  let out = ref None in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if p grid.(i).(j) && Option.is_none !out then
        out := Some Vec2.{ x = i; y = j }
    done
  done;
  !out

let unwrap (x: 'a option): 'a =
  match x with
  | Some x -> x
  | None -> failwith "unwrap"

let dirs = [
  Vec2.{ x = -1; y = 0 };
  Vec2.{ x = 0; y = 1 };
  Vec2.{ x = 1; y = 0 };
  Vec2.{ x = 0; y = -1 };
]

let inbounds (grid: 'a array array) (pos: Vec2.t): bool =
  let (n, m) = (Array.length grid, Array.length grid.(0)) in
  0 <= pos.x && pos.x < n && 0 <= pos.y && pos.y < m

let get (grid: 'a array array) (pos: Vec2.t): 'a =
  grid.(pos.x).(pos.y)

let calc_dist (is_wall: bool array array) (start: Vec2.t): int array array =
  let (n, m) = (Array.length is_wall, Array.length is_wall.(0)) in
  let vis = Array.make_matrix n m false in
  let dist = Array.make_matrix n m 0 in
  let q = Queue.create () in
  Queue.push start q;
  vis.(start.x).(start.y) <- true;
  while not (Queue.is_empty q) do
    let u = Queue.pop q in
    dirs |> List.iter (fun dir ->
      let v = Vec2.add u dir in
      if inbounds is_wall v && not vis.(v.x).(v.y) && not (get is_wall v) then begin
        vis.(v.x).(v.y) <- true;
        (* Printf.printf "%d %d %d %d\n%!" u.x u.y v.x v.y;
        Printf.printf "%b\n%!" (inbounds is_wall v); *)
        dist.(v.x).(v.y) <- dist.(u.x).(u.y) + 1;
        Queue.push v q
      end
    )
  done;
  dist

let good_threshold = 100
let max_dist = 20

let rec range (lo: int) (hi: int): int list =
  if lo > hi then []
  else lo :: range (lo + 1) hi

let cp (xs: 'a list) (ys: 'b list): ('a * 'b) list =
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)

let () =
  record_backtrace true;
  let grid = read_grid () in
  (* Array.iter (fun row -> Array.iter (Printf.printf "%c") row; print_newline ()) grid; *)
  let (n, m) = (Array.length grid, Array.length grid.(0)) in
  let (is_wall: bool array array) = map2d (fun x -> x = '#') grid in
  (* Array.iter (fun row -> Array.iter (fun b -> Printf.printf "%d" (if b then 1 else 0)) row; print_newline ()) is_wall; *)
  let start = find grid (fun c -> c = 'S') |> unwrap in
  let goal = find grid (fun c -> c = 'E') |> unwrap in
  let start_dist = calc_dist is_wall start in
  let goal_dist = calc_dist is_wall goal in
  let no_shortcut_dist = get start_dist goal in
  Printf.printf "Normal distance: %d\n" no_shortcut_dist;
  let all_offsets = cp (range (-max_dist) max_dist) (range (-max_dist) max_dist)
    |> List.filter (fun (dx, dy) -> abs dx + abs dy <= max_dist)
    |> List.map (fun (dx, dy) -> Vec2.{ x = dx; y = dy }) in
  let ans = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      let p = Vec2.{ x = i; y = j } in
      if not (get is_wall p) then
        all_offsets |> List.iter (fun dir ->
          let q = Vec2.add p dir in
          if inbounds is_wall q && not (get is_wall q) then
            let shortcut_dist = get start_dist p + abs (dir.x) + abs (dir.y) + get goal_dist q in
            if shortcut_dist <= no_shortcut_dist - good_threshold then begin
              incr ans;
              (* Printf.printf "%d %d\n" dir.x dir.y;
              Printf.printf "Shortcut found at (%d, %d) to (%d, %d) that saves %d\n" i j q.x q.y (no_shortcut_dist - shortcut_dist) *)
            end
        )
    done
  done;
  Printf.printf "%d\n" !ans
