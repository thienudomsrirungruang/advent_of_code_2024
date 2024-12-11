open Printexc

let rec prep (x: 'a) (t: int) (ys: 'a list): 'a list =
  if t = 0 then
    ys
  else
    x :: prep x (t - 1) ys

let rec convert (xs: int list) (running: int): int list =
  match xs with
  | [] -> []
  | x :: y :: ys -> convert ys (running + 1) |> prep (-1) y |> prep running x 
  | x :: [] -> [] |> prep running x

let () =
  record_backtrace true;
  let input = read_line () in
  let xs = input |> String.to_seq |> List.of_seq |> List.map (fun c -> Char.code c - Char.code '0') in
  let init = convert xs 0 |> Array.of_list in
  let n = Array.length init in
  let a = ref 0 in
  let b = ref (n - 1) in
  while a < b do
    if init.(!a) <> -1 then
      a := !a + 1
    else if init.(!b) = -1 then
      b := !b - 1
    else
      begin
        let temp = init.(!a) in
        init.(!a) <- init.(!b);
        init.(!b) <- temp;
      end
  done;
  let checksum = init |> Array.combine (Array.init n (fun x -> x)) |> Array.map (fun (x, y) -> if y = -1 then 0 else x * y) |> Array.fold_left (+) 0 in
  Printf.printf "%d\n" checksum
