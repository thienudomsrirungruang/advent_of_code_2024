
open Printexc
open Option

type 'a string_state_m = string -> 'a * string

let (>>=) (xm: 'a string_state_m) (f: 'a -> 'b string_state_m): 'b string_state_m = (fun s -> let (v, state) = xm s in f v state) 
let return (v: 'a): 'a string_state_m = (fun s -> (v, s))
let get: string string_state_m = (fun s -> (s, s))
let put (s: string): unit string_state_m = (fun _ -> ((), s))

let read_all_lines (): string list =
  let rec f () =
    try
      let line = read_line () in
      line :: f ()
    with
    | End_of_file -> []
  in
  f ()

let slice (s: string) (a: int) (b: int): string =
  if a >= b then ""
  else if b > String.length s then String.sub s a (String.length s - a)
  else String.sub s a (b - a)

let chomp (x: int): unit string_state_m =
  get >>= fun xs ->
  put (slice xs x (String.length xs)) >>= fun () ->
  return ()

let try_chomp (p: string) (good: 'a string_state_m) (bad: 'a string_state_m): 'a string_state_m =
  get >>= fun xs ->
    if slice xs 0 (String.length p) = p then
      chomp (String.length p) >>= fun () ->
      good
    else
      bad

let rec eat_integer (): int option string_state_m =
  let rec eat_int_string (): string string_state_m =
    get >>= fun xs ->
      match xs with
        | "" -> return ""
        | _ ->
          let c = String.get xs 0 in
            if c >= '0' && c <= '9' then
              chomp 1 >>= fun () ->
              eat_int_string () >>= fun ys ->
              return (String.make 1 c ^ ys)
            else return ""
  in
  eat_int_string () >>= fun out ->
  if out = "" then return None
  else return (Some (int_of_string out))

let rec calc_ans (): int string_state_m = 
  let fail () = (chomp 1 >>= fun () -> calc_ans () >>= fun x -> return x) in
  get >>= fun inp ->
  match inp with
    | "" -> return 0
    | _ ->
      try_chomp "mul(" 
      (
        eat_integer () >>= fun arg1 ->
        match arg1 with
          | None -> fail ()
          | Some arg1 ->
            try_chomp ","
            (
              eat_integer () >>= fun arg2 ->
              match arg2 with
                | None -> fail ()
                | Some arg2 ->
                  try_chomp ")" 
                  (
                    calc_ans () >>= fun rest ->
                    return (arg1 * arg2 + rest)
                  )
                  (fail ())
            )
            (fail ())
      )
      (fail ())

let read_all_stdin (): string =
  List.fold_right (fun x acc -> x ^ "\n" ^ acc) (read_all_lines ()) ""

let () =
  record_backtrace true;
  let s = read_all_stdin () in
  let (ans, _) = calc_ans () s in
  print_int ans;
  print_newline ()
