open Printexc

type input_data = {
  grid: char array array;
  sequence: char list;
}

module CharMap = Map.Make(Char)

let replacements =
  CharMap.of_seq (List.to_seq [
    ('@', "@.");
    ('#', "##");
    ('.', "..");
    ('O', "[]");
  ])

let replace_line (s: string) =
  s |> String.to_seq |> List.of_seq |> List.map (fun c -> CharMap.find c replacements |> String.to_seq |> List.of_seq) |> List.concat |> List.to_seq |> String.of_seq

let read_input (): input_data =
  let grid_lines = ref [] in
  let sequence = ref [] in
  let reading_grid = ref true in
  while !reading_grid do
    let line = read_line () in
    if line = "" then
      reading_grid := false
    else
      grid_lines := (replace_line line |> String.to_seq |> Array.of_seq) :: !grid_lines
  done;
  let grid = Array.of_list (List.rev !grid_lines) in
  let reading_sequence = ref true in
  while !reading_sequence do
    try
      let line = read_line () in
      sequence := List.concat [!sequence; String.to_seq line |> List.of_seq]
    with End_of_file -> reading_sequence := false
  done;
  { grid; sequence = !sequence }

module Vec2 = struct
  type t = { x: int; y: int }

  let add a b = { x = a.x + b.x; y = a.y + b.y }
  let compare a b = match compare a.x b.x with
    | 0 -> compare a.y b.y
    | n -> n
end

module CharMap = Map.Make(Char)

let directions = CharMap.of_seq (List.to_seq [
  ('<', Vec2.{ x = 0; y = -1 });
  ('>', Vec2.{ x = 0; y = 1 });
  ('^', Vec2.{ x = -1; y = 0 });
  ('v', Vec2.{ x = 1; y = 0 });
])

let inbounds (grid: 'a array array) (pos: Vec2.t) =
  pos.x >= 0 && pos.x < Array.length grid && pos.y >= 0 && pos.y < Array.length grid.(0)

let push (grid: char array array) (dir: Vec2.t) =
  let push_from (pos: Vec2.t) =
    assert (grid.(pos.x).(pos.y) = '@');
    let push_positions = ref [] in
    let queue = Queue.create () in
    Queue.push pos queue;
    let vis = Array.make_matrix (Array.length grid) (Array.length grid.(0)) false in
    vis.(pos.x).(pos.y) <- true;
    let good = ref true in
    while not (Queue.is_empty queue) do
      let cur = Queue.pop queue in
      push_positions := (cur, grid.(cur.x).(cur.y)) :: !push_positions;
      let next = Vec2.add cur dir in
      let c = grid.(next.x).(next.y) in
      if c = '#' then begin
        good := false;
        Queue.clear queue
      end
      else if not vis.(next.x).(next.y) && (c = '[' || c = ']') then begin
        vis.(next.x).(next.y) <- true;
        Queue.push next queue;
        let d = if c = '[' then 1 else -1 in
        if not vis.(next.x).(next.y+d) then begin
          vis.(next.x).(next.y+d) <- true;
          Queue.push Vec2.{ x = next.x; y = next.y+d } queue
        end
      end
    done;
    if !good then begin
      List.iter (fun p -> let pos: Vec2.t = fst p in grid.(pos.x).(pos.y) <- '.') !push_positions;
      List.iter (fun (pos, c) -> let next = Vec2.add pos dir in grid.(next.x).(next.y) <- c) !push_positions
    end
  in
  let pushed = ref false in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      if not !pushed && grid.(i).(j) = '@' then begin
        push_from { x = i; y = j };
        pushed := true;
      end
    done
  done

let () =
  record_backtrace true;
  let input = read_input () in
  let grid = input.grid in
  let sequence = input.sequence in
  List.iter (fun dir -> push grid (CharMap.find dir directions)) sequence;
  (* Array.iter (fun row -> Array.iter (Printf.printf "%c") row; print_newline ()) grid *)
  let ans = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      if grid.(i).(j) = '[' then ans := !ans + 100 * i + j
    done
  done;
  Printf.printf "%d\n" !ans
