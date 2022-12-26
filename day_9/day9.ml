open Printf
let file = "input.txt"
let ic = open_in file

module IntPairs =
  struct 
    type t = int * int
    let compare (a,b) (c,d) = 
      (match Int.compare a c with
      | 0 -> Int.compare b d
      | e -> e)
  end

module S = Set.Make(IntPairs)

exception Bad_input
let ic2 = Scanf.Scanning.from_channel ic
let rec readAll ic = 
  try
  let chunk = try Scanf.bscanf ic "%c %d\n" (fun a b -> (a,b)) with 
      | Scanf.Scan_failure s -> let _ = printf "%s" s in raise Bad_input
      | End_of_file -> raise End_of_file in 
    chunk::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic2

type dir = R | L | U | D
let cToDir a =
  match a with
  | 'R' -> R
  | 'L' -> L
  | 'U' -> U
  | 'D' -> D
  | _ -> raise Bad_input

let dirToC a =
  match a with
  | R -> 'R'
  | L -> 'L'
  | U -> 'U'
  | D -> 'D'

let typeInput = List.map (fun (a,b) -> (cToDir a,b)) l2


let hPos = ref(0,0)
let hPrev = ref(0,0)
let lPos = ref(0,0)
let allLPos = ref(S.add (0,0) S.empty)

let doInstr (i,n) = 
  let rec doOne j = 
    let isLInRange (a,b) (c,d) = (Int.abs (a-c) <= 1 && Int.abs (b-d) <= 1) in
    (match j with
    | 0 -> ()
    | a -> 
      let () = hPrev := !hPos in
      let (hx,hy) = !hPos in
      let () = 
        (match i with
        | R -> hPos := (hx+1,hy)
        | L -> hPos := (hx-1,hy)
        | U -> hPos := (hx,hy+1)
        | D -> hPos := (hx,hy-1)) in
      let () = if not(isLInRange !lPos !hPos) then lPos := !hPrev in
      let () = allLPos := S.add !lPos !allLPos in
      doOne (j-1)) in
  doOne n

let rec doAllInstr tI =
  match tI with
  | [] -> ()
  | a::b -> let () = doInstr a in
    doAllInstr b

let () = doAllInstr typeInput

let () = printf "total part 1: %d\n" (S.fold (fun a b -> b + 1) !allLPos 0)

(* result 6059 *)


  let allLPos2 = ref(S.singleton (0,0))
  let typeInput = List.map (fun (a,b) -> (cToDir a,b)) l2
  
  let isLInRange (a,b) (c,d) = (Int.abs (a-c) <= 1 && Int.abs (b-d) <= 1)
  let calcNextPos (tcx,tcy) (lcx,lcy) = 
    let newx = if tcx>lcx then tcx-1 else if tcx<lcx then tcx+1 else tcx in 
    let newy = if tcy>lcy then tcy-1 else if tcy<lcy then tcy+1 else tcy in
    (newx,newy)
  
  let moved_p_n (a,b) (c,d) = if (Int.abs (a-c) = 1 && Int.abs (b-d) = 1) then Some (c-a,d-b) else None
  let determineNewPos (currx,curry) (lastCurrx, lastCurry) = 
    if isLInRange (currx,curry) (lastCurrx,lastCurry) then (currx,curry) else 
      calcNextPos (currx,curry) (lastCurrx,lastCurry)
  
  let rec doInstr2 list lastCurr = 
    (match list with 
    | [] -> []
    | a::b -> let (newx,newy) = determineNewPos a lastCurr in
              let () = if b = [] then allLPos2 := S.add (newx,newy) !allLPos2 else () in
              (newx,newy)::(doInstr2 b (newx,newy)))
    
  let doInstr ((x,y)::list) i = 
    let (a,b) = 
    (match i with 
        | R -> (x+1,y)
        | L -> (x-1,y)
        | U -> (x,y+1)
        | D -> (x,y-1)) in
    (a,b)::(doInstr2 list (a,b))
  
  let rec repeatInstr list (i,cnt) = 
    (match cnt with
    | 0 -> list
    | a -> repeatInstr (doInstr list i) (i,cnt-1))
  
  let starterList = [(0,0);(0,0);(0,0);(0,0);(0,0);(0,0);(0,0);(0,0);(0,0);(0,0)]
  
  let _ = List.fold_left repeatInstr starterList typeInput 
  
  let () = printf "total part 2: %d\n" (S.fold (fun a b -> b + 1) !allLPos2 0)
  
  (* result 2514 *)