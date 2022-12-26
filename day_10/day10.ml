open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

type instr = Add of int | Noop

let instrs = List.map (fun a -> 
  if String.get a 0 = 'n' then Noop 
  else let (b::c::d) = String.split_on_char ' ' a in Add (int_of_string c)) l2

let modifyTotal (clk,reg,total) = if (clk - 20) mod 40 = 0 then 
let () = if (clk-1) mod 40 = 0 then printf "\n" else () in 
  (total + (reg * clk)) else total

let (clk1,reg1,total1) = List.fold_left (fun (clk,reg,total) instr -> 
  (match instr with
  | Noop -> (clk+1,reg,modifyTotal (clk+1,reg,total))
  | Add v -> 
    let total = modifyTotal (clk+1,reg,total) in (clk+2,reg+v,modifyTotal (clk+2,reg+v,total)))) (1,1,0) instrs 
(* weird clock off by 1 error compensated in starting tupil *)

let () = printf "total:%d\n" total1

(* result: 14060 *)

let _ = List.fold_left (fun (clk,reg) instr -> 
  let () = if (clk-1) mod 40 = 0 then printf "\n" else () in
  let () = if Int.abs(((clk-1) mod 40) - reg) <= 1 then printf "#" else printf "." in
  (match instr with
  | Noop -> (clk+1,reg)
  | Add v -> 
    let () = if clk mod 40 = 0 then printf "\n" else () in
    let () = if Int.abs(((clk) mod 40) - reg) <= 1 then printf "#" else printf "." in
    (clk+2,reg+v))) (1,1) instrs 

let () = printf "\n"

(* output: PAPKFKEJ *)

