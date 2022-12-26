open Printf
open Option

module IntPairs =
  struct 
    type t = int * int
    let compare (a,b) (c,d) = 
      (match Int.compare a c with
      | 0 -> Int.compare b d
      | e -> e)
  end

module M = Map.Make(IntPairs)
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

type tile = Wall | Tile

exception Wonky_input
let rec parseLine l m i = 
  (match l with 
  | [] -> raise Wonky_input
  | a::b -> if a = "" then let c::d = b in (c,m) 
    else let (newm,_) = (String.fold_left (fun (m2,j) ch -> if ch = '.' then ((M.add (i,j) Tile m2),j+1) else if ch = '#' then ((M.add (i,j) Wall m2),j+1) else (m2,j+1))) (m,0) a in 
    parseLine b newm (i+1)
  )
let (instrs,mapd) = parseLine l2 M.empty 0

type dir = R | D | L | U
type spinDir = Rs | Ls
type instr = Spin of spinDir | Move of int

let rec parseInstrs s = 
  if String.length s = 0 then [] else
  let spini = (match (String.index_opt s 'L',String.index_opt s 'R') with
  | (None,None) -> String.length s
  | (Some i,None) -> i
  | (None, Some i) -> i
  | (Some i, Some j) -> Int.min i j) in
  if spini = 0 then (if String.get s 0 = 'L' then Spin(Ls) else Spin(Rs))::(parseInstrs (String.sub s 1 ((String.length s)-1)))
else (Move(int_of_string (String.sub s 0 spini)))::(parseInstrs (String.sub s spini ((String.length s)-spini)))

let inst = parseInstrs instrs

exception Missed_range
let findElem (r,c) dir m =
  try let _ = M.find (r,c) m in (r,c)
  with Not_found -> (match dir with
  | R -> if r>=0&&r<=99 then (r,50) else if r>=100&&r<=199 then (r,0) else raise Missed_range
  | D -> if c>=0&&c<=49 then (100,c) else if c>=50&&c<=149 then (0,c) else raise Missed_range
  | L -> if r>=0&&r<=49 then (r,149) else if r>=50&&r<=149 then (r,99) else if r>=150&&r<=199 then (r,49) else raise Missed_range
  | U -> if c>=0&&c<=49 then (199,c) else if c>=50&&c<=99 then (149,c) else if c>=100&&c<=149 then (49,c) else raise Missed_range)

let calcSpin dir spin = 
  (match spin with
  | Rs -> (match dir with
    | R -> D
    | D -> L
    | L -> U
    | U -> R)
  | Ls -> (match dir with
  | R -> U
  | D -> R
  | L -> D
  | U -> L))

let rec oneInst ((r,c),dir) inst = 
  (match inst with
  | Spin a -> ((r,c),(calcSpin dir a))
  | Move a -> if a = 0 then ((r,c),dir) else 
    let (r3,c3) = (match dir with | R -> (r,c+1) | D -> (r+1,c) | L -> (r,c-1) | U -> (r-1,c)) in 
    let (r2,c2) = findElem (r3,c3) dir mapd in
    if M.find (r2,c2) mapd = Wall then ((r,c),dir)
    else oneInst ((r2,c2),dir) (Move(a-1)))

let dirToC d = 
  (match d with
  | U -> 'U'
  | L -> 'L'
  | D -> 'D'
  | R -> 'R')


let ((finalR,finalC),finalDir) =  List.fold_left (fun pos inst -> oneInst pos inst) ((0,50),R) inst

let () = printf "result part 1: %d\n" (1000*(finalR+1)+4*(finalC+1)+(match finalDir with | R -> 0 | D -> 1 | L -> 2 | U -> 3))
 (* result: 95358 *)

let findElem2 (r,c) dir m =
 try let _ = M.find (r,c) m in ((r,c),dir)
 with Not_found -> (match dir with
 | R -> if r>=0&&r<=49 then (((49-r)+100,99),L)
   else if r>=50&&r<=99 then ((49,(r-50)+100),U)
   else if r>=100&&r<=149 then ((49-(r-100),149),L)
   else if r>=150&&r<=199 then ((149,(r-150)+50),U) else raise Missed_range
 | D -> if c>=0&&c<=49 then ((0,c+100),D)
   else if c>=50&&c<=99 then (((c-50)+150,49),L)
   else if c>=100&&c<=149 then (((c-100)+50,99),L) else raise Missed_range
 | L -> if r>=0&&r<=49 then (((49-r)+100,0),R)
   else if r>=50&&r<=99 then ((100,(r-50)),D)
   else if r>=100&&r<=149 then ((49-(r-100),50),R)
   else if r>=150&&r<=199 then ((0,r-150+50),D) else raise Missed_range
 | U -> if c>=0&&c<=49 then ((c+50,50),R)
   else if c>=50&&c<=99 then (((c-50)+150,0),R)
   else if c>=100&&c<=149 then ((199,c-100),U) else raise Missed_range)

let rec oneInst2 ((r,c),dir) inst = 
  (match inst with
  | Spin a -> ((r,c),(calcSpin dir a))
  | Move a -> if a = 0 then ((r,c),dir) else 
    let (r3,c3) = (match dir with | R -> (r,c+1) | D -> (r+1,c) | L -> (r,c-1) | U -> (r-1,c)) in 
    let ((r2,c2),dir2) = findElem2 (r3,c3) dir mapd in
    if M.find (r2,c2) mapd = Wall then ((r,c),dir)
    else oneInst2 ((r2,c2),dir2) (Move(a-1)))


let ((finalR,finalC),finalDir) =  List.fold_left (fun pos inst -> oneInst2 pos inst) ((0,50),R) inst

let () = printf "result part 2: %d\n" (1000*(finalR+1)+4*(finalC+1)+(match finalDir with | R -> 0 | D -> 1 | L -> 2 | U -> 3))

(* result: 144361 *)