open Printf
let file = "input.txt"

module SS = Set.Make(Char)

let ic = open_in file

let rec readAll ic l = 
  let line = try input_line ic
    with e -> let () = close_in_noerr ic in "!" in
    if line = "!" then
      l 
    else readAll ic (line::l)

let l2 = readAll ic []

let rec list_from_string s = 
  match s with
  | "" -> []
  | _ -> (String.get s 0)::(list_from_string (String.sub s 1 (String.length s - 1)))

exception NoMatch

let rec doAll l ttl = 
  match l with
  | [] -> ttl
  | a::b -> 
    let alen2 = (String.length a)/2 in
    let (st,li) = (SS.of_list (list_from_string (String.sub a 0 alen2)),list_from_string(String.sub a alen2 alen2)) in
    let rec check_set st l = 
      match l with
      | [] -> raise NoMatch
      | a::b -> 
        match SS.find_opt a st with
        | None -> check_set st b
        | Some c -> if Char.code c < 97 then (Char.code c) - 38
        else Char.code c - 96 in
    doAll b (ttl + (check_set st li))

let res = doAll l2 0

let () = printf "result 1: %d\n" res

(* result 1: 7737 *)

exception NoMult3
let rec triple l =
  match l with
  | [] -> []
  | a::b::c::d -> [a;b;c]::(triple d)
  | _ -> raise NoMult3

let l3 = triple l2
let rec doAll2 l ttl =
  match l with
  | [] -> ttl
  | [a;b;c]::d -> 
    let st = SS.of_list (list_from_string a) in
    let rec set_list st l= 
      match l with
      | [] -> []
      | a::b -> 
        match SS.find_opt a st with
        | None -> set_list st b
        | Some c -> a::(set_list st b) in
    let st2 = SS.of_list (set_list st (list_from_string b)) in
    let rec check_set st l = 
      match l with
      | [] -> raise NoMatch
      | a::b -> 
        match SS.find_opt a st with
        | None -> check_set st b
        | Some c -> if Char.code c < 97 then (Char.code c) - 38
        else Char.code c - 96 in
    doAll2 d (ttl + (check_set st2 (list_from_string c)))
  | _ -> raise NoMult3

let rec2 = doAll2 l3 0

let () = printf "result 2: %d\n" rec2

(* result 2: 2697 *)