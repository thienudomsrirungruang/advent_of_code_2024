open Printexc

let read_all_lines (): string list =
  let rec f () =
    try
      let line = read_line () in
      line :: f ()
    with
    | End_of_file -> []
  in
  f ()

type pinput = {
  edges: (int * int) list;
  seqs: int list list
}

let rec split_pred (f: 'a -> bool) (xs: 'a list): 'a list * 'a list =
  match xs with
  | [] -> failwith "no element that satisfies predicate"
  | y::ys ->
    if f y then
      ([], ys)
    else
      let (as_, bs) = split_pred f ys in
      (y::as_, bs)

let parse (lines: string list): pinput =
  let (first_part, second_part) = split_pred (fun x -> x = "") lines in
  let edges = List.map (fun line -> match String.split_on_char '|' line with
      | [x; y] -> (int_of_string x, int_of_string y)
      | _ -> failwith "invalid edge") first_part in
  let seqs = List.map (fun line -> List.map int_of_string (String.split_on_char ',' line)) second_part in
  {edges = edges; seqs = seqs}

let check (edges: (int * int) list) (seq: int list): bool =
  let sl = seq |> Array.of_list in
  let n = Array.length sl in
  let bad p q = match List.find_opt (fun (x, y) -> y = p && x = q) edges with
    | None -> false
    | Some _ -> true in
  let res = ref true in
  for i = 0 to n-1 do
    for j = i+1 to n-1 do
      if bad sl.(i) sl.(j) then
        res := false
    done
  done;
  !res

let middle (a: 'a list): 'a =
  let n = List.length a in
  List.nth a (n / 2)

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> parse in
  let good_seq = List.filter (check inputs.edges) inputs.seqs in
  List.fold_right (+) (List.map middle good_seq) 0 |> print_int;
  print_newline ()

