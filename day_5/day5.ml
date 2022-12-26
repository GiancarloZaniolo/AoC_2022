open Printf
let file = "input.txt"

let ic = open_in file

let rec readCount ic c = 
  if c > 0 then
  try let line = input_line ic in 
    line::(readCount ic (c-1))
  with End_of_file -> []
else []

let boxesInput = readCount ic 8

module M = Map.Make(Int)

let x = M.empty
let x = M.add 9 [] x let x = M.add 1 [] x let x = M.add 2 [] x
let x = M.add 3 [] x let x = M.add 4 [] x let x = M.add 5 [] x
let x = M.add 6 [] x let x = M.add 7 [] x let x = M.add 8 [] x

let rec boxLine s i m = 
  if (i*4-3) >= (String.length s) then m
  else let c = String.get s (i*4-3) in if
  (int_of_char c >= 65) && (int_of_char c <= 90) 
  then boxLine s (i+1) (M.add i (c::(M.find i m)) m)
  else boxLine s (i+1) m

let rec mkBoxMap m l = 
  match l with
  | [] -> m
  | a::b -> mkBoxMap (boxLine a 1 m) b

let x1 = mkBoxMap x boxesInput
let print_clist b = List.iter (fun a -> printf "%c " a) b
let x2 = M.map (List.rev) x1

let _ = input_line ic
let _ = input_line ic

let ic2 = Scanf.Scanning.from_channel ic
exception Bad_input
let rec readAll ic = 
  try
  let chunk = try Scanf.bscanf ic "move %d from %d to %d\n" (fun a b c -> (a,b,c)) with 
      | Scanf.Scan_failure s -> let _ = printf "%s" s in raise Bad_input
      | End_of_file -> raise End_of_file in 
    chunk::(readAll ic)
  with End_of_file -> []

let l2 = readAll ic2

let rec get_n l i =
  match l with
  | [] -> ([],[])
  | a::b -> if i > 0 
    then let (c,d) = (get_n b (i-1)) in (a::c,d)
    else ([],l)

let do_trans m (a,b,c) = 
  let (d,e) = get_n (M.find b m) a in
  let m = M.add b e m in
  M.add c ((List.rev d)@(M.find c m)) m

let rec do_all_trans m l = 
  match l with 
  | [] -> m
  | a::b -> do_all_trans (do_trans m a) b

let res1 = do_all_trans x2 l2

let print_top l = 
  match l with
  | [] -> ()
  | a::b -> printf "%c" a

let () = M.iter (fun i l -> print_top l) res1
let () = printf "\n"

(* answer 1: FWNSHLDNZ *)

let do_trans2 m (a,b,c) = 
  let (d,e) = get_n (M.find b m) a in
  let m = M.add b e m in
  M.add c (d@(M.find c m)) m

  let rec do_all_trans2 m l = 
    match l with 
    | [] -> m
    | a::b -> do_all_trans2 (do_trans2 m a) b

let res2 = do_all_trans2 x2 l2

let () = M.iter (fun i l -> print_top l) res2
let () = printf "\n"

(* answer 2: RNRGDNFQG *)


