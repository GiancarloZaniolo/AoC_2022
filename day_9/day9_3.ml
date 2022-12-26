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







open Printf
let file = "input3.txt"
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

(* let () = List.iter (fun (a,b) -> printf "(%c,%d) " a b) l2 *)

(* let typeInput = ref(List.map (fun (a,b) -> (cToDir a,b)) l2) *)
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
      (* let (x,y) = !hPos in let (v,w) = !lPos in let (t,u) = !hPrev in
      let () = printf "((%d,%d),(%d,%d),(%d,%d)),%B " x y v w t u (not(isLInRange !lPos !hPos)) in *)
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

(* let () = printf "\n\n" *)

(* let () = S.iter (fun (a,b) -> printf "(%d,%d) " a b) !allLPos *)
let () = printf "total: %d\n" (S.fold (fun a b -> b + 1) !allLPos 0)



(* when the following know moves diagonally, something different has to happen *)
(* if other chunk moved diagonally, this chunk must move diagonally the same way *)
(* for tail, just use pattern matching to check if last in list and set big set if so *)

let fullSeq = [ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0)]
let fullSeqPrev = [ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0);ref(0,0)]

let allLPos2 = ref(S.singleton (0,0))

(* if not in range than check if moved 1,1 compared to prev, if so match, if not go to prev *)
exception Weird_iteration
let doInstr (dir,cnt) = 
  let isLInRange (a,b) (c,d) = (Int.abs (a-c) <= 1 && Int.abs (b-d) <= 1) in
  let moved_p_n (a,b) (c,d) = if (Int.abs (a-c) = 1 && Int.abs (b-d) = 1) then
    let () = printf "~%d,%d~" (c-a) (d-b) in
    Some (c-a,d-b) else None in
  let oneIter (hPrev::lp) (hPos::l) = 
    let () = let () = List.iter (fun a -> let (b,c) = !a in printf "(%d,%d)" b c) (hPrev::lp) in printf "\n" in
    let () = hPrev := !hPos in
      let (hx,hy) = !hPos in
      let () = 
        (match dir with
        | R -> hPos := (hx+1,hy)
        | L -> hPos := (hx-1,hy)
        | U -> hPos := (hx,hy+1)
        | D -> hPos := (hx,hy-1)) in
    let rec otherIters lp l pp p = 
      let () = printf "iter" in
      (match (l,lp) with
      | ([],[]) -> let () = printf "iter2" in ()
      | ((a::b),(ax::bx)) -> 
          let () = ax := !a in
          let () = if not(isLInRange !a !p) then 
            (match (moved_p_n !pp !p) with
            | Some (x,y) -> let (a0,a1) = !a in a := (a0+x,a1+y)
            | None -> a := !pp) in 
          if b = [] then 
            let () = allLPos2 := S.add !a !allLPos2 in
          otherIters bx b ax a) in
      otherIters lp l hPrev hPos
  in
  for i = 0 to cnt do
    oneIter fullSeqPrev fullSeq
  done

let rec doAllInstr tI =
  match tI with
  | [] -> ()
  | a::b -> let () = doInstr a in
    doAllInstr b

let () = doAllInstr typeInput

let () = printf "total: %d\n" (S.fold (fun a b -> b + 1) !allLPos2 0)

let typeInput = List.map (fun (a,b) -> (cToDir a,b)) l2