
let parse_int_list line =
  List.map int_of_string (List.filter (fun x -> String.length x > 0) (String.split_on_char ' ' line))

let read_all_lines (): string list =
  let rec f () =
    try
      let line = read_line () in
      line :: f ()
    with
    | End_of_file -> []
  in
  f ()

let parse_input (): int list list =
  let rec parser lines = match lines with
    | [] -> []
    | l::ls -> parse_int_list l :: parser ls
  in
  parser (read_all_lines ())

let list_init (xs : int list): int list =
  List.rev (List.tl (List.rev xs))
  
let deltas (xs: int list): int list =
  List.map2 (fun x y -> y - x) (list_init xs) (List.tl xs)

let sign (x: int): int =
  if x > 0 then 1 else if x < 0 then -1 else 0

let is_safe (xs: int list): bool =
  let ds = deltas xs in
  let signs = List.map sign ds in
  let all_same_dir = List.for_all (fun x -> x == List.hd signs) signs in
  let all_correct_magnitude = List.for_all (fun x -> (1 <= abs x) && (abs x <= 3)) ds in
  all_same_dir && all_correct_magnitude

let () =
  let inp = parse_input () in
  (* List.iter (fun xs -> print_int (if (is_safe xs) then 1 else 0); print_newline ()) inp *)
  let answer = List.fold_left (fun acc xs -> acc + if is_safe xs then 1 else 0) 0 inp in
  print_int answer;
  print_newline ()

