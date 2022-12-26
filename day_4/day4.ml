open Printf
let file = "input.txt"
let ic = open_in file
let ic2 = Scanf.Scanning.from_channel ic

exception Bad_input

let rec readAll ic = 
  try
  let chunk = try Scanf.bscanf ic "%d-%d,%d-%d\n" (fun a b c d -> (a,b,c,d)) with 
      | Scanf.Scan_failure s -> let _ = printf "%s" s in raise Bad_input
      | End_of_file -> raise End_of_file in 
    chunk::(readAll ic)
  with End_of_file -> []

let l2 = readAll ic2

let res = List.fold_left (fun e (a,b,c,d) -> if ((a <= c) && (b >= d)) || ((c <= a) && (d >= b)) 
  then e + 1 else e) 0 l2

let _ = printf "%d\n" res
(* result 1: 599 *)

let res2 = List.fold_left (fun e (a,b,c,d) -> if ((b <= d) && (b >= c)) || ((a <= d) && (b >= c)) || ((a <= c) && (b >= d))
  then e + 1 else e) 0 l2

let _ = printf "%d\n" res2
(* result 2: 928 *)
