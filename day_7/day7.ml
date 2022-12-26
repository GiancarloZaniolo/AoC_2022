open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []

let l2 = readAll ic

type instr_parse = Cdd of string | Cdb | Ls | Dirt of string | Filet of (int * string)

exception Bad_input
let rec tok_inpt l = 
  match l with
  | [] -> []
  | a::b -> match String.get a 0 with
    | '$' -> (match String.get a 2 with
      | 'c' -> (match String.get a 5 with
        | '.' -> Cdb::(tok_inpt b)
        | _ -> (Cdd (String.sub a 5 (String.length a - 5)))::(tok_inpt b) )
      | _ -> Ls::(tok_inpt b) )
    | 'd' -> (Dirt (String.sub a 4 (String.length a - 4)))::(tok_inpt b)
    | _ -> let d::e::f = String.split_on_char ' ' a in
      (Filet (int_of_string d,e))::(tok_inpt b)

let rec print_toks l =
  match l with
  | [] -> ()
  | a::b -> let () = match a with
    | Cdd a -> printf "CD %s\n" a
    | Cdb -> printf "CD ..\n"
    | Ls -> printf "LS\n"
    | Dirt a -> printf "DIR %s\n" a
    | Filet (a,b) ->  printf "FILE %d %s\n" a b 
  in print_toks b


let a::lTok = tok_inpt l2

module M = Map.Make(String)

type dirElem = Dir of dirElem M.t | File of int

let rec popDir l map = 
  match !l with
  | [] -> let () = l := [] in map
  | a::b -> (match a with
    | Filet (i,s) -> 
      let () = l := b in
      popDir l (M.add s (File (i)) map)
    | Dirt s -> 
      let () = l := b in
      popDir l (M.add s (Dir (M.empty)) map)
    | _ -> map )


let rec do_instr map l = 
  match !l with
  | [] -> let () = l := [] in map
  | a::b -> (match a with
    | Cdb -> let () = l := b in map
    | Ls -> let () = l := b in 
      do_instr (popDir l map) l
    | Cdd c -> let () = l := b in 
        let Dir(findD) = (M.find c map) in do_instr (M.add c (Dir(do_instr findD l)) map) l
    | _ -> raise Bad_input )

let instrMap = do_instr M.empty (ref(lTok))

let rec printDirs map spaces = 
  let rec print_spaces spaces = 
    (match spaces with
    | 0 -> ()
    | a -> let () = printf " " in print_spaces (spaces - 1)) in
  M.iter 
    (fun s a -> let () = print_spaces spaces in
        (match a with 
        | File i -> printf "file %s %d\n" s i
        | Dir m -> let () = printf "dir %s:\n" s in printDirs m (spaces+1)))
    map

let sumTot = ref(0)
let rec bigDirMap s psMap = 
  let acc = M.fold
    (fun s a acc -> 
      (match a with
      | File i -> (acc + i)
      | Dir m -> let macc = bigDirMap s m in (acc + macc))) psMap 0 in 
  let () = if acc <= 100000 then sumTot := !sumTot + acc in acc

let currentAllocated = bigDirMap "/" instrMap

let () = printf "total part 1: %d\n" !sumTot 

(* answer part 1: 1844187 *)

let ce = ref(currentAllocated)
let rec bigDirMap s psMap = 
  let acc = M.fold
    (fun s a acc -> 
      (match a with
      | File i -> (acc + i)
      | Dir m -> let macc = bigDirMap s m in (acc + macc))) psMap 0 in 
  let () = if acc <= !ce && 70000000 - currentAllocated + acc >= 30000000 then ce := acc in acc

let temp = bigDirMap  "/" instrMap
let () = printf "min part 2: %d\n" !ce

(* answer part 2: 4978279 *)