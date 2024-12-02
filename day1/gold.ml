
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

let count (xs: int list) (y: int) =
  List.fold_left (fun acc x -> if x = y then acc + 1 else acc) 0 xs

let () =
  let inp = parse_input () in
  let pairs = List.map (fun (xs: int list) -> (List.nth xs 0, List.nth xs 1)) inp in
  let (xs, ys) = List.split pairs in
  let xs = List.sort compare xs in
  let ys = List.sort compare ys in
  let result = List.fold_left (fun acc x -> acc + x * count ys x) 0 xs in
  print_int result;
  print_newline ()
