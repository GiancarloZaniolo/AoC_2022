open Printf
let file = "input.txt"

let ic = open_in file
let l = [0]

let rec readAll ic (i::l) = 
  let line = try input_line ic
    with e -> let () = close_in_noerr ic in "!" in
    if line = "!" then
      i::l 
    else if line <> "" then
      readAll ic ((int_of_string(line)+i)::l)
    else 
      readAll ic (0::i::l)

let l2 = readAll ic l


let rec big3 l (a,b,c) = 
  match l with
  | [] -> (a,b,c)
  | d::e -> 
    if d > c then
      if d > b then
        if d > a then
          big3 e (d,a,b)
        else
          big3 e (a,d,b)
      else
        big3 e (a,b,d)
    else
      big3 e (a,b,c)

let (a,b,c) = big3 l2 (0,0,0)

let () = printf "result 1: %d\n" a
(* result 1: 70369 *)

let () = printf "result 2: %d\n" (a+b+c)
(* result 2: 203002 *)