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

let move (inputs: int array) (num: int): unit =
  let n = Array.length inputs in
  let l = ref (-1) in
  let r = ref (-1) in
  for i = 0 to n - 1 do
    if inputs.(i) = num then
      begin
        if !l = -1 then l := i;
        r := i
      end
  done;
  if !l = -1 then failwith "number not found";
  let len = !r - !l + 1 in
  let new_pos = ref (-1) in
  for i = 0 to !l do
    if !new_pos = -1 then
      begin
        let good = ref true in
        for j = 0 to len - 1 do
          if inputs.(i + j) <> -1 then
            good := false
        done;
        if !good then
          new_pos := i 
      end
  done;
  if !new_pos <> -1 then
    begin
      for i = !l to !r do
        inputs.(i) <- -1
      done;
      for i = 0 to len - 1 do
        inputs.(!new_pos + i) <- num
      done
    end

let () =
  record_backtrace true;
  let input = read_line () in
  let xs = input |> String.to_seq |> List.of_seq |> List.map (fun c -> Char.code c - Char.code '0') in
  let init = convert xs 0 |> Array.of_list in
  let n = Array.length init in
  let mx = Array.fold_left max 0 init in
  for i = mx downto 0 do
    move init i
  done;
  (* Array.iter (fun x -> Printf.printf "%d " x) init;
  Printf.printf "\n"; *)
  let checksum = init |> Array.combine (Array.init n (fun x -> x)) |> Array.map (fun (x, y) -> if y = -1 then 0 else x * y) |> Array.fold_left (+) 0 in
  Printf.printf "%d\n" checksum
