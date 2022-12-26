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

let currArr = Array.make 10 (0,0)
let prevArr = Array.make 10 (0,0)
let allLPos2 = ref(S.singleton (0,0))

let doInstr i = 
  (* set all prev values to current *)
  let isLInRange (a,b) (c,d) = (Int.abs (a-c) <= 1 && Int.abs (b-d) <= 1) in
  let moved_p_n (a,b) (c,d) = if (Int.abs (a-c) = 1 && Int.abs (b-d) = 1) then Some (c-a,d-b) else None in
  let _ = Array.fold_left (fun a b -> let () = Array.set prevArr a (currArr.(a)) in a+1) 0 prevArr in
  Array.fold_left 
  (fun a b -> 
    let () = 
    (match a with
    | 0 -> let (x,y) = Array.get currArr 0 in
      (match i with 
      | R -> Array.set currArr 0 (x+1,y)
      | L -> Array.set currArr 0 (x-1,y)
      | U -> Array.set currArr 0 (x,y+1)
      | D -> Array.set currArr 0 (x,y-1))
    | _ -> let (currax,curray) = Array.get currArr a in 
           let prevan = Array.get prevArr (a-1) in 
           let curran = Array.get currArr (a-1) in
           if isLInRange (currax,curray) curran then 
            (match (moved_p_n prevan curran) with
            | None -> Array.set currArr a (Array.get prevArr (a-1))
            | Some (x,y) -> Array.set currArr a (currax+x,curray+y))) in
    let () = if a = Array.length currArr - 1 then  allLPos2 := S.add (Array.get currArr a) !allLPos2 in
    a+1) 0 currArr

let doInstrs (i,cnt) = 
  for j = 0 to cnt do
    let () = Array.iter (fun (a,b) -> printf "(%d,%d)" a b) currArr in
    let () = printf "\n" in
    let _ = doInstr i in ()
  done

  let rec doAllInstr tI =
    match tI with
    | [] -> ()
    | a::b -> let () = doInstrs a in
      doAllInstr b

let () = doAllInstr typeInput

let () = printf "total: %d\n" (S.fold (fun a b -> b + 1) !allLPos2 0)


(* see where it eventually goes, print intermediate states *)