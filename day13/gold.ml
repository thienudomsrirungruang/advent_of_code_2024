open Printexc

module Vec2 = struct
  type t = { x: int; y: int }
  let add (a: t) (b: t): t = { x = a.x + b.x; y = a.y + b.y }
  let sub (a: t) (b: t): t = { x = a.x - b.x; y = a.y - b.y }
  let mul (a: t) (k: int): t = { x = a.x * k; y = a.y * k }
end

type mach = {
  button_a: Vec2.t;
  button_b: Vec2.t;
  goal: Vec2.t;
}

let offset = 10000000000000
(* let offset = 0 *)

let parse (): mach option =
  try
    let button_a = Scanf.scanf "Button A: X+%d, Y+%d\n" (fun x y -> Vec2.{ x; y }) in
    let button_b = Scanf.scanf "Button B: X+%d, Y+%d\n" (fun x y -> Vec2.{ x; y }) in
    let goal = Scanf.scanf "Prize: X=%d, Y=%d\n" (fun x y -> Vec2.{ x = x + offset; y = y + offset }) in
    begin
      try 
        Scanf.scanf("\n") ()
      with End_of_file -> ();
    end;
    Some { button_a; button_b; goal }
  with
    End_of_file -> None

let parse_all () =
  let rec f () =
    match parse () with
    | Some mach -> mach :: f ()
    | None -> []
  in
  f ()

let lim = 100

let find_answer (m: mach): int =
  assert (Vec2.mul m.button_a m.button_b.x <> Vec2.mul m.button_b m.button_a.x);
  let det = m.button_a.x * m.button_b.y - m.button_a.y * m.button_b.x in
  let xn = m.goal.x * m.button_b.y - m.goal.y * m.button_b.x in
  let yn = m.button_a.x * m.goal.y - m.goal.x * m.button_a.y in
  Printf.printf "det = %d, xn = %d, yn = %d\n" det xn yn;
  if det = 0 then 0
  else if xn mod det <> 0 || yn mod det <> 0 then 0
  else
    let x = xn / det in
    let y = yn / det in
    if x < 0 || y < 0 then 0
    else 3 * x + y

let () =
  record_backtrace true;
  let machines = parse_all () in
  List.map find_answer machines |> List.fold_left (+) 0 |> print_int;
  print_newline()
