open Printf
let file = "input.txt"
let ic = open_in file
let ic2 = Scanf.Scanning.from_channel ic
exception Bad_input
let rec readAll ic = 
  try
  let chunk = try Scanf.bscanf ic "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d\n" (fun a b c d -> (a,b,c,d)) with 
      | Scanf.Scan_failure s -> let _ = printf "%s" s in raise Bad_input
      | End_of_file -> raise End_of_file in 
    chunk::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic2

module M = Map.Make(Int)
module S = Set.Make(Int)

let rec findMinMax l (min,max) =
  (match l with
  | [] -> (min,max)
  | (a,b,c,d)::e -> let sDist = Int.abs(a-c)+Int.abs(b-d) in findMinMax e (Int.min (b-sDist) min,Int.max (b+sDist) max) )

let rec makeRow l i = 
  let rec insertElems l =
    (match l with
    | [] -> []
    | (a,b,c,d)::e -> let sDist = Int.abs(a-c)+Int.abs(b-d) in 
      if Int.abs (i-b) <= sDist then (a-(sDist-(Int.abs(i-b))),a+(sDist-(Int.abs(i-b))))::(insertElems e) else insertElems e) in 
    let rec noOverlap l = 
      (match l with
      | [] -> []
      | [a] -> [a]
      | (a,b)::(c,d)::e -> if c<=b then if d<=b then noOverlap ((a,b)::e) else noOverlap ((a,d)::e) else (a,b)::(noOverlap((c,d)::e))) in
    noOverlap (List.sort (fun (a,b) (c,d) -> Int.compare a c) (insertElems l)) 

let rec makeBeaconSet l m =
  (match l with
  | [] -> m
  | (a,b,c,d)::e -> let s = (try M.find d m with Not_found -> S.empty) in let s = S.add c s in makeBeaconSet e (M.add d s m)) 

let sumRow l m i = (List.fold_left (fun a (b,c) -> a + (c-b+1)) 0 l) - (try (S.cardinal (M.find i m)) with Not_found -> 0)


let temp = makeRow l2 2000000
let beacons = makeBeaconSet l2 M.empty
let () = printf "result part 1: %d\n" (sumRow temp beacons 2000000)

(* let (min,max) = findMinMax l2 (Int.max_int,Int.min_int) *)

let allRows = Array.make 4000001 [(0,0)]
let allRows = Array.mapi (fun i a -> makeRow l2 i) allRows

let ((ii,ci),(t1,t2)::b) = Array.fold_left (fun ((ii,ci),b) a -> if (List.length a) > 1 then ((ci,ci+1),a) else ((ii,ci+1),b)) ((-1,0),[]) allRows

let () = printf "result part 2: %d\n" ((t2+1)*4000000+ii)
