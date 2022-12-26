open Printf
let file = "input.txt"
let ic = open_in file

let line = input_line ic (* one line *)

let rec list_from_string s = 
  match s with
  | "" -> []
  | _ -> (String.get s 0)::(list_from_string (String.sub s 1 (String.length s - 1)))

let lInput = list_from_string line

let rec findIdx l (a,b,c) i = 
  match l with
  | [] -> -1
  | d::e -> if d <> a && d <> b && d <> c && a <> b && a <> c && c <> b then i
    else findIdx e (b,c,d) (i+1) 

let () = printf "idx: %d\n" (findIdx lInput ('t','t','t') 1)

(* answer 1896 *)

let rec insertNewl l i c = 
  match l with 
  | [] -> (c,l)
  | a::b -> if i = 0 then (c,l) else insertNewl b (i-1) (a::c)

let (origQ,lInput) = insertNewl lInput 13 []

let rec remLast a = 
  match a with
  | [] -> []
  | a::[] -> []
  | a::b -> a::(remLast b)

let rec unique_list l = 
  match l with
  | [] -> true
  | c::d -> if List.fold_left (fun b a -> if b then b else c = a) false d 
    then false 
    else unique_list d

let rec findIdx2 l q i = 
  match l with
  | [] -> -1
  | d::e -> if unique_list (d::q) then i
    else findIdx2 e (d::(remLast q)) (i+1) 

let () = printf "idx2: %d\n" (findIdx2 lInput origQ 14)

(* answer 3452 *)

