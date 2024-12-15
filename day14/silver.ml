open Printexc

module Vec2 = struct
  type t = { x: int; y: int }
  let add (a: t) (b: t): t = { x = a.x + b.x; y = a.y + b.y }
  let sub (a: t) (b: t): t = { x = a.x - b.x; y = a.y - b.y }
  let mul (a: t) (k: int): t = { x = a.x * k; y = a.y * k }
end

type robot = {
  p: Vec2.t;
  v: Vec2.t;
}

let parse (): robot option =
  try
    Scanf.scanf "p=%d,%d v=%d,%d\n" (fun px py vx vy -> Some { p = { x = px; y = py }; v = { x = vx; y = vy } })
  with
    End_of_file -> None
    
let parse_all (): robot list =
  let rec f () =
    match parse () with
    | Some r -> r :: f ()
    | None -> []
  in
  f ()

let w = 101
let h = 103
let time = 100

let wrap (v: Vec2.t): Vec2.t =
  { x = ((v.x mod w) + w) mod w; y = ((v.y mod h) + h) mod h }

let quadrant (v: Vec2.t): int =
  if v.x == w / 2 || v.y == h / 2 then -1
  else
  if v.x < w / 2 then
    if v.y < h / 2 then 0 else 1
  else
    if v.y < h / 2 then 2 else 3

let () =
  record_backtrace true;
  let robots = parse_all () in
  let finals = List.map (fun r -> wrap (Vec2.add r.p (Vec2.mul r.v time))) robots in
  let counts = Array.init 4 (fun _ -> 0) in
  List.iter (fun f ->
    let q = quadrant f in
    if q <> -1 then counts.(q) <- counts.(q) + 1
  ) finals;
  Array.iteri (fun i c -> Printf.printf "Quadrant %d: %d\n" i c) counts;
  let prod = Array.fold_left ( * ) 1 counts in
  Printf.printf "%d\n" prod
  
