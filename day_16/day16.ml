open Printf
module M = Map.Make(String)
module S = Set.Make(String)
let file = "input.txt"
let ic = open_in file
let rec readLines ic =
  try let a = input_line ic in  a::(readLines ic)
with End_of_file -> let () = close_in ic in []
let l2 = readLines ic

let rec parseLines l = 
  (match l with
  | [] -> []
  | a::b -> 
    let c::d::e = String.split_on_char ';' a in
    let (pos,flw) = Scanf.sscanf c "Valve %s has flow rate=%d" (fun a b -> (a,b)) in
    let others = String.split_on_char ' ' (String.sub d 23 (String.length d - 23)) in
     let others = List.fold_right (fun a b -> if String.length a = 0 then b else a::b) [] others in
    (pos,flw,others)::(parseLines b))

let rec cleanParse l = (* sadge *)
  (match l with 
  | [] -> []
  | (a,b,c)::d -> 
    let c2 = List.fold_right 
    (fun a b -> if String.length a = 0 then b else if String.length a = 3 then (String.sub a 0 2)::b  else a::b) 
    c [] in
    (a,b,c2)::(cleanParse d))

let l3 = cleanParse(parseLines l2)

let rec printParse l = 
  (match l with
  | [] -> ()
  | (a,b,c)::d -> let () = printf "p:%s f:%d " a b in let () = List.iter (fun a -> printf "%s," a) c in let () = printf "\n"
in printParse d)

let sorted = List.sort (fun (a,b,c) (d,e,f) -> String.compare a d) l3

let rec sToIndf l i m = 
  (match l with
  | [] -> m
  | (a,b,c)::d -> sToIndf d (i+1) (M.add a i m))
let sToInd s = M.find s (sToIndf sorted 0 M.empty)
let indToS i = Array.get (Array.of_list (List.map (fun (a,b,c) -> a) sorted)) i
let graph = Array.of_list (List.map (fun (a,b,c) -> (List.map (fun d -> sToInd d) c)) sorted)
let v_q = Queue.create ()
let visited = Array.make (List.length sorted) (-1)


(* makes a matrix containing the shortest distance from every vert to every other vert *)
let allDist = Array.make_matrix (List.length sorted) (List.length sorted) (-1)
let rec getAllDist i =
  try
  let () = Queue.clear v_q in
  let () = List.iter (fun a -> Queue.add (i,a) v_q) (graph.(i)) in 
  let () = allDist.(i).(i) <- 0 in 
  let () = while not (Queue.is_empty v_q) do
    let (elemsrc,elemdst) = Queue.pop v_q in 
    let () = allDist.(i).(elemdst) <- (allDist.(i).(elemsrc) + 1) in 
    List.iter (fun a -> Queue.add (elemdst,a) v_q) (List.filter (fun b -> (allDist.(i).(b) = -1)) (graph.(elemdst)))
  done in 
  getAllDist (i+1)
  with e -> ()

let () = getAllDist 0

(* contains (index original graph,flow rate) of all non-zero valves *)
let nonz = List.filter (fun (i,b) -> not (b=0)) (List.mapi (fun i (a,b,c) -> (i,b)) sorted)
(* contains distances from every non-zero valve to every other *)
let graph2 = let l = (List.length nonz) in Array.make_matrix l l (-1)
let rec fillInGraph newi l = 
  (match l with
  | [] -> ()
  | (oldi,a)::b -> let _ = List.fold_left 
      (fun newj (oldj,b) -> let () = graph2.(newi).(newj) <- (allDist.(oldi).(oldj)) in newj+1) 
      0 nonz in fillInGraph (newi+1) b)
let () = fillInGraph 0 nonz


(* contains (index new graph,flow rate) of all non-zero valves *)
let wtArr = Array.of_list (List.mapi (fun ii (i,a) -> (ii,a)) nonz)
let bestSteam = ref(0)
let rec calcBest currP currT vted rate total = 
  let count = Array.fold_left 
  (fun a (i,b) -> 
    let disti = graph2.(currP).(i) in
    if (30 > currT+disti+1) && (vted.(i) = false) then 
    let () = vted.(i) <- true in
    let () = calcBest i (currT+disti+1) vted (rate+b) (total+((rate)*(disti+1))) in
    let () = vted.(i) <- false in 
    a+1 else a) 0 wtArr in
  if (count = 0) && ((total+rate*(30-currT)) > !bestSteam) then bestSteam := (total+rate*(30-currT)) 
  else ()
  (* I don't think b is distance *)

(* makes a lists of the distance from a to all non-zero nodes *)
let rec findADists l = 
  (match l with
  | [] -> []
  | (i,b)::c -> (allDist.(0).(i))::(findADists c))
let nodeADists = findADists nonz
let vted = Array.make (List.length nonz) false
let rec allCalcBests l i =
  (match l with
  | [] -> ()
  | a::b -> 
    let () = vted.(i) <- true in 
    let (_,wti) = wtArr.(i) in 
    let () = calcBest i (a+1) vted wti 0 in (*currP currT vted rate total*)
    let () = vted.(i) <- false in
    allCalcBests b (i+1))

let () = allCalcBests nodeADists 0

let () = printf "result part 1: %d\n%!" !bestSteam

(* result 1: 1754 *)

let vted2 = Array.make (List.length nonz) false
let bestSteam2 = ref(0)
let rec calcBest2 (cp1,ct1) (cp2,ct2) vted rate total = 
  let count = if cp1 = -1 then (* if is first node of first elem *)
    let (cnt,_) =    
    List.fold_left 
      (fun (cnt,i) disti -> 
        if (26 > ct1+disti+1) && (vted.(i) = false) then
          let () = vted.(i) <- true in 
          let () = calcBest2 (i,(disti+1)) (cp2,ct2) vted (rate) (total) in 
          let () = vted.(i) <- false in 
        (cnt+1,i+1) else (cnt,i+1)) 
      (0,0) nodeADists in cnt
  else if cp2 = -1 then (* if is first node of second elem *)
    let (cnt,_) = List.fold_left
      (fun (cnt,i) disti -> 
        if (26 > ct2+disti+1) && (vted.(i) = false) then
          let () = vted.(i) <- true in 
          let () = calcBest2 (cp1,ct1) (i,(disti+1)) vted (rate) (total) in (*still no total calc, at time 0*)
          let () = vted.(i) <- false in 
        (cnt+1,i+1) else (cnt,i+1))
      (0,0) nodeADists in cnt
  else if ct1 <= ct2 then (* if ct1 comes before ct2 iterate thru all of its values *)
    Array.fold_left
      (fun a (i,b) ->
        let disti = graph2.(cp1).(i) in
        let (_,wti) = wtArr.(cp1) in
        if (26 > ct1+disti+1) && (vted.(i) = false) then
          let () = vted.(i) <- true in 
          let () = calcBest2 (i,ct1+disti+1) (cp2,ct2) vted (rate+wti) (total+(((Int.min (ct1+disti+1) ct2)-ct1)*(rate+wti))) in
          let () = vted.(i) <- false in
        (a+1) else a)
      0 wtArr
  else 
    Array.fold_left
      (fun a (i,b) ->
        let disti = graph2.(cp2).(i) in
        let (_,wti) = wtArr.(cp2) in
        if (26 > ct2+disti+1) && (vted.(i) = false) then
          let () = vted.(i) <- true in 
          let () = calcBest2 (cp1,ct1) (i,ct2+disti+1) vted (rate+wti) (total+(((Int.min ct1 (ct2+disti+1))-ct2)*(rate+wti))) in
          let () = vted.(i) <- false in
        (a+1) else a)
      0 wtArr
  in 
    if count = 0 then
    (match (ct1 = Int.max_int,ct2 = Int.max_int) with
    | (false,false) -> (* check which one was just viewed, set to int max, set total, recalls*)
      if ct1 <= ct2 then let (_,wti) =  wtArr.(cp1) in
        calcBest2 (cp1,Int.max_int) (cp2,ct2) vted (rate+wti) (total+((rate+wti)*(ct2-ct1))) 
      else let (_,wti) =  wtArr.(cp2) in
        calcBest2 (cp1,ct1) (cp2,Int.max_int) vted (rate+wti) (total+(rate+wti)*(ct1-ct2))
    | (true,false) -> (* check if final better *)
      let (_,wti) = wtArr.(cp2) in
      if (total+(rate+wti)*(26-ct2)) > !bestSteam2 then 
        bestSteam2 := (total+(rate+wti)*(26-ct2))
    | (false,true) -> (* check if final better *)
      let (_,wti) = wtArr.(cp1) in
      if (total+(rate+wti)*(26-ct1)) > !bestSteam2 then
        bestSteam2 := (total+(rate+wti)*(26-ct1))
    | (true,true) ->  printf "should never reach here\n") 
    else ()

let () = calcBest2 (-1,0) (-1,0) vted2 0 0 

let () = printf "result part 2: %d\n" !bestSteam2
(* result 2: 2474 *)
