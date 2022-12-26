open Printf
open Option
let file = "input.txt"
let ic = open_in file

(* This code is extremely bad because I did not properly read the instructions
   and wound up hacking stuff together *)

module IntPairs =
  struct 
    type t = int * int
    let compare (a,b) (c,d) = 
      (match Int.compare a c with
      | 0 -> Int.compare b d
      | e -> e)
  end

module M = Map.Make(IntPairs)
module S = Set.Make(IntPairs)

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

let (_,sParsed) = List.fold_left (fun (i,s) a -> let (j,s2) = String.fold_left (fun (j,s) b -> (j+1,if b = '#' then S.add (i,j) s else s)) (0,s) a in (i+1,s2)) (0,S.empty) l2

let rec findE s (er,ec) iter  = if iter>=4 then (er,ec) else 
  try let _ = S.find (er-1,ec+1) s in findN s (er,ec) (iter+1) with Not_found -> 
  try let _ = S.find (er+1,ec+1) s in findN s (er,ec) (iter+1) with Not_found ->
  try let _ = S.find (er,ec+1) s in findN s (er,ec) (iter+1) with Not_found -> (er,ec+1) 
and findW s (er,ec) iter = if iter>=4 then (er,ec) else
  try let _ = S.find (er-1,ec-1) s in findE s (er,ec) (iter+1) with Not_found -> 
  try let _ = S.find (er+1,ec-1) s in findE s (er,ec) (iter+1) with Not_found -> 
  try let _ = S.find (er,ec-1) s in findE s (er,ec) (iter+1) with Not_found -> (er,ec-1)
and findS s (er,ec) iter = if iter>=4 then (er,ec) else
  try let _ = S.find (er+1,ec-1) s in findW s (er,ec) (iter+1) with Not_found ->
  try let _ = S.find (er+1,ec+1) s in findW s (er,ec) (iter+1) with Not_found ->
  try let _ = S.find (er+1,ec) s in findW s (er,ec) (iter+1) with Not_found -> (er+1,ec)
and findN s (er,ec) iter = if iter>=4 then (er,ec) else
  try let _ = S.find (er-1,ec-1) s in findS s (er,ec) (iter+1) with Not_found ->
  try let _ = S.find (er-1,ec+1) s in findS s (er,ec) (iter+1) with Not_found -> 
  try let _ = S.find (er-1,ec) s in findS s (er,ec) (iter+1) with Not_found -> (er-1,ec)

let opOnIter i = 
  (match i mod 4 with
  | 0 -> findN
  | 1 -> findS
  | 2 -> findW
  | 3 -> findE)

let checkAllAdj s (r,c) =
  try let _ = S.find (r+1,c+1) s in false with Not_found ->
  try let _ = S.find (r+1,c) s in false with Not_found ->
  try let _ = S.find (r+1,c-1) s in false with Not_found ->
  try let _ = S.find (r,c+1) s in false with Not_found ->
  try let _ = S.find (r,c-1) s in false with Not_found ->
  try let _ = S.find (r-1,c+1) s in false with Not_found ->
  try let _ = S.find (r-1,c) s in false with Not_found ->
  try let _ = S.find (r-1,c-1) s in false with Not_found -> true

let calcOneIter s iter = 
  let calcNextStep (er,ec) (m,s) = 
    let newpos = if checkAllAdj s (er,ec) then (er,ec) else  (opOnIter iter) s (er,ec) 0 in
    try let _ = M.find newpos m in (M.add newpos None m,s) with Not_found -> (M.add newpos (Some (er,ec)) m,s) in 
  let (m2,s2) = S.fold calcNextStep s (M.empty,s) in 
  let (sOld,sNew) = M.fold (fun pos posRem (sPrev,sNext) -> (match posRem with | None -> (sPrev,sNext) | Some posRem2 -> (S.remove posRem2 sPrev,S.add pos sNext))) m2 (s,S.empty) in
  S.fold (fun remainPos sNew2 -> S.add remainPos sNew2) sOld sNew

let doNRounds s n = 
  let rec rounds s2 i = if i = n then s2 else 
    rounds (calcOneIter s2 i) (i+1) in 
  rounds s 0

let finalSet = doNRounds sParsed 10

let (maxr,maxc,minr,minc) = S.fold (fun (r,c) (mar,mac,mir,mic) -> (Int.max mar r,Int.max mac c,Int.min mir r, Int.min mic c)) finalSet (let (r,c) = S.min_elt finalSet in (r,c,r,c))

let () = printf "result part 1: %d\n" (((maxr-minr+1)*(maxc-minc+1))-(S.cardinal finalSet))

(* result: 4172 *)

let rec tryRounds s i = 
  let s2 = calcOneIter s i in 
  if S.equal s s2 then i else tryRounds s2 (i+1)

let () = printf "result part 2: %d\n" ((tryRounds sParsed 0)+1)

(* result: 942 *)