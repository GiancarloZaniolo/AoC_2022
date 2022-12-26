open Printf
(* code jank but works *)
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

let decodec c = (match c with 
| '2' -> 2
| '1' -> 1
| '0' -> 0
| '-' -> -1
| '=' -> -2)

let convert1 s = 
  let (res,_) = String.fold_right 
  (fun c (acc,mul) -> 
    ((decodec c)*mul+acc,mul*5)) s (0,1) in res

let res = List.fold_left (fun res b -> convert1 b + res) 0 l2

let rec largestPow n v = if v/n = 0 then n/5 else largestPow (n*5) v
let rec toPow5 v p acc = 
  if p = 0 then acc else
  toPow5 (v mod p) (p/5) 
  (match (v/p) with
  | 0 -> String.cat acc "0"
  | 1 -> String.cat acc "1"
  | 2 -> String.cat acc "2"
  | 3 -> String.cat acc "3"
  | 4 -> String.cat acc "4")

let rec add1Str s = 
  (match (String.get s (String.length s - 1)) with
  | '=' -> String.cat (String.sub s 0 (String.length s - 1)) "-"
  | '-' -> String.cat (String.sub s 0 (String.length s - 1)) "0"
  | '0' -> String.cat (String.sub s 0 (String.length s - 1)) "1"
  | '1' -> String.cat (String.sub s 0 (String.length s - 1)) "2"
  | '2' -> try String.cat (add1Str (String.sub s 0 (String.length s - 1))) "="
with Invalid_argument e -> "1=")

let final = 
String.fold_left (fun acc c -> 
  (match c with
  | '0' -> String.cat acc "0"
  | '1' -> String.cat acc "1"
  | '2' -> String.cat acc "2"
  | '3' -> String.cat (add1Str acc) "="
  | '4' -> String.cat (add1Str acc) "-") ) "" (toPow5 res (largestPow 1 res) "")

let () = printf "result part 1: %s\n" final 
(* result: 2=000=22-0-102=-1001 *)