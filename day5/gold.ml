open Printexc
open Hashtbl

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

let rec make_closure (edges: (int * int) list): (int * int) list =
  let rec f (edges: (int * int) list) : (int * int) list =
    let el = Array.of_list edges in
    let n = Array.length el in
    let res = ref edges in
    let finished = ref true in
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        if snd el.(i) = fst el.(j) then
          if not (List.mem (fst el.(i), snd el.(j)) !res) then
            begin
              (* Printf.printf "adding %d -> %d\n" (snd el.(i)) (fst el.(j));
              Printf.printf "%d\n" (if List.mem (snd el.(i), fst el.(j)) !res then 1 else 0);
              List.iter (fun (x, y) -> Printf.printf "%d -> %d " x y) !res;
              print_newline (); *)
              res := (fst el.(i), snd el.(j)) :: !res;
              finished := false
            end
      done
    done;
    if !finished then
      !res
    else
      f !res
  in
  f edges

let get_closure (edges: (int * int) list) (seq: int list): (int * int) list =
  let relevant_edges = List.filter (fun (x, y) -> List.mem x seq && List.mem y seq) edges in
  make_closure relevant_edges

let is_mid (closure: (int * int) list) (n: int) (x: int) =
  let lcnt = List.length (List.filter (fun (a, b) -> b = x) closure) in
  let rcnt = List.length (List.filter (fun (a, b) -> a = x) closure) in
  lcnt = rcnt && lcnt + rcnt = n - 1

let rec find_mid (edges: (int * int) list) (seq: int list): int =
  let closure = get_closure edges seq in
  let n = List.length seq in
  let res = ref None in
  List.iter (fun x -> if is_mid closure n x then res := Some x) seq;
  match !res with
    | None -> failwith "no middle"
    | Some x -> x

let () =
  record_backtrace true;
  let inputs = read_all_lines () |> parse in
  let bad_seq = List.filter (fun x -> check inputs.edges x |> not) inputs.seqs in
  (* List.iter (fun x -> 
    let closure = get_closure inputs.edges x in
    List.iter (fun (x, y) -> Printf.printf "%d -> %d " x y) closure;
    print_newline ()
  ) bad_seq; *)
  List.fold_right (+) (List.map (find_mid inputs.edges) bad_seq) 0 |> print_int;
  print_newline ()

