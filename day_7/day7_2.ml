open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []

let l2 = readAll ic

type instr_parse = Cdd of string | Cdb | Ls | Dirt of string | Filet of (int*string)

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
(* let () = print_toks lTok *)

module M = Map.Make(String) 

type dirElem = Dir of dirElem M.t | File of int

let rec popDir l map = 
  match l with
  | [] -> (map,[])
  | a::b -> (match a with
    | Filet (i,s) -> 
      (* let () = printf "filet %s\n" s in *)
      popDir b (M.add s (File (i)) map)
    | Dirt s -> 
      (* let () = printf "dirt %s\n" s in *)
      popDir b (M.add s (Dir (M.empty)) map)
    | _ -> (map,l)) 


let rec do_instr map l = 
  (* let () =  M.iter (fun s a -> printf "thing %s " s) map in let () = printf "\n" in *)
  match l with
  | [] -> (map,[])
  | a::b -> (match a with
    | Cdb -> (map,b)
    | Ls -> let (m2,l2) = popDir b map in do_instr m2 l2
    | Cdd c -> try
        let Dir mc = M.find c map in 
        let (mc2,l2) = do_instr mc b in 
          do_instr (M.add c (Dir mc2) map) l2
    with Not_found -> let () = printf "finding %s\n" c in raise Not_found
    | _ -> raise Bad_input )

let (instrMap,l) = do_instr M.empty lTok

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

(* let () = let () = printDirs instrMap 0 in printf "\n" *)

let rec bigDirMap s psMap nMap = 
  let acc = M.fold
    (fun s a acc -> 
      (match a with
      | File i -> (acc + i)
      | Dir m -> let macc = bigDirMap s m nMap in (acc + macc))) psMap 0 in 
  (* let () = printf "acc %s: %d\n" s acc in *)
  let () = (nMap := if acc <= 100000 then M.add s acc !nMap else !nMap) in acc

let nMap = ref (M.empty)
let (temp) = bigDirMap "/" instrMap nMap
let () = M.iter (fun s i -> printf "%s: %d\n" s i) !nMap
let () = printf "total: %d\n" (M.fold (fun s i acc -> acc+i) !nMap 0)

(* 1761942 too low *)
(* 1761942 *)
  
(* I have no clue what is wrong with my func... *)

