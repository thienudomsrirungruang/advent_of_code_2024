
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

let () =
  let inp = parse_input () in
  let pairs = List.map (fun (xs: int list) -> (List.nth xs 0, List.nth xs 1)) inp in
  let (xs, ys) = List.split pairs in
  let xs = List.sort compare xs in
  let ys = List.sort compare ys in
  let result = List.fold_left2 (fun acc x y -> acc + abs (x - y)) 0 xs ys in
  print_int result;
  print_newline ()
