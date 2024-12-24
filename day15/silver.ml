
type input_data = {
  grid: char array array;
  sequence: char list;
}

let read_input (): input_data =
  let grid_lines = ref [] in
  let sequence = ref [] in
  let reading_grid = ref true in
  while !reading_grid do
    let line = read_line () in
    if line = "" then
      reading_grid := false
    else
      grid_lines := (String.to_seq line |> Array.of_seq) :: !grid_lines
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
    let num_boxes = ref 0 in
    let move_pos = Vec2.add pos dir in
    let next_pos = ref move_pos in
    while grid.(!next_pos.x).(!next_pos.y) = 'O' do
      next_pos := Vec2.add !next_pos dir;
      incr num_boxes;
    done;
    if grid.(!next_pos.x).(!next_pos.y) = '.' then begin
      grid.(!next_pos.x).(!next_pos.y) <- 'O';
      grid.(move_pos.x).(move_pos.y) <- '@';
      grid.(pos.x).(pos.y) <- '.';
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
  let input = read_input () in
  let grid = input.grid in
  let sequence = input.sequence in
  List.iter (fun dir -> push grid (CharMap.find dir directions)) sequence;
  (* Array.iter (fun row -> Array.iter (Printf.printf "%c") row; print_newline ()) grid *)
  let ans = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      if grid.(i).(j) = 'O' then ans := !ans + 100 * i + j
    done
  done;
  Printf.printf "%d\n" !ans
