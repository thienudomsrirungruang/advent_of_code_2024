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

let parse (): mach option =
  try
    let button_a = Scanf.scanf "Button A: X+%d, Y+%d\n" (fun x y -> Vec2.{ x; y }) in
    let button_b = Scanf.scanf "Button B: X+%d, Y+%d\n" (fun x y -> Vec2.{ x; y }) in
    let goal = Scanf.scanf "Prize: X=%d, Y=%d\n" (fun x y -> Vec2.{ x; y }) in
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
  let best = ref max_int in
  for i = 0 to lim do
    for j = 0 to lim do
      let final = Vec2.add (Vec2.mul m.button_a i) (Vec2.mul m.button_b j) in
      if final = m.goal then
        best := min !best (3 * i + j)
    done
  done;
  if !best = max_int then 0 else !best

let () =
  record_backtrace true;
  let machines = parse_all () in
  List.map find_answer machines |> List.fold_left (+) 0 |> print_int;
  print_newline()
