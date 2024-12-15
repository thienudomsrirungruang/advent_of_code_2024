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
let total_time = w * h

let wrap (v: Vec2.t): Vec2.t =
  { x = ((v.x mod w) + w) mod w; y = ((v.y mod h) + h) mod h }

let print_state (positions: Vec2.t list) =
  let grid = Array.make_matrix h w ' ' in
  List.iter (fun Vec2.{x; y} -> grid.(y).(x) <- '#') positions;
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      print_char grid.(y).(x)
    done;
    print_newline ()
  done

let total_dist (positions: Vec2.t list) =
  let n = List.length positions in
  let xs = List.map (fun (p: Vec2.t) -> p.x) positions |> Array.of_list in
  let ys = List.map (fun (p: Vec2.t) -> p.y) positions |> Array.of_list in
  Array.sort compare xs;
  Array.sort compare ys;
  let q = n / 4 in
  let r = 3 * n / 4 in
  xs.(r) - xs.(q) + ys.(r) - ys.(q)

let () =
  record_backtrace true;
  let robots = parse_all () in
  let min_dist = ref max_int in
  for i = 0 to total_time do
    let positions = List.map (fun r -> wrap (Vec2.add r.p (Vec2.mul r.v i))) robots in
    let d = total_dist positions in
    if d < !min_dist then 
      begin
        min_dist := d;
        print_int i; print_newline ();
        print_state positions;
      end
  done
