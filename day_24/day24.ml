open Printf

module IntPairs =
  struct 
    type t = int * int
    let compare (a,b) (c,d) = 
      (match Int.compare a c with
      | 0 -> Int.compare b d
      | e -> e)
  end
module IntPair3 =
  struct 
    type t = int * int * int 
    let compare (a,b,c) (d,e,f) = 
      (match Int.compare a d with
      | 0 -> (match Int.compare b e with
        | 0 -> Int.compare c f
        | h -> h)
      | g -> g)
  end
module M = Map.Make(IntPairs)
module M3 = Map.Make(IntPair3)
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

type ground_t = R | L | U | D
let groundtToC e = (match e with|R->'R'|L->'L'|U->'U'|D->'D')

let startPos = ref(-1,-1)
let endPos = ref(-1,-1)
let (parsedM,_) = List.fold_left
  (fun (m,i) e -> 
    let (mfinal,_) = String.fold_left 
    (fun (m,j) c -> (match c with
      | '>' -> (M.add (i,j) [R] m,j+1)
      | '<' -> (M.add (i,j) [L] m,j+1)
      | '^' -> (M.add (i,j) [U] m,j+1)
      | 'v' -> (M.add (i,j) [D] m,j+1)
      | _ -> let() = if i=0&&c='.' then startPos := (i,j) in 
      let () = if i=(List.length l2-1)&&c='.' then endPos := (i,j) in 
      (m,j+1))) (m,0) e in (mfinal,i+1)) (M.empty,0) l2

let (sr,sc) = !startPos
let (er,ec) = !endPos

let calcNextBPos m = 
  M.fold 
    (fun (a,b) l m2 -> 
      List.fold_left 
        (fun m3 c -> let npos = (match c with 
          | R -> if b = ec then (a,1) else (a,b+1)
          | L -> if b = 1 then (a,ec) else (a,b-1)
          | U -> if a = 1 then (er-1,b) else (a-1,b)
          | D -> if a = (er-1) then (1,b) else (a+1,b)) in
          let lTemp = try M.find npos m3 with Not_found -> [] in M.add npos (c::lTemp) m3) (*come back to here*)
        m2 l) m M.empty

let roundArr = Array.make ((er-1)*ec) parsedM
(* rounds are 0-indexed *)
let _ = Array.fold_left (fun (i,m) b -> let () = roundArr.(i) <- m in (i+1,calcNextBPos m)) (0,parsedM) roundArr

let height = List.init (er-1) (fun i -> i+1) let width = List.init (ec) (fun i -> i+1)

let (graph,_) = Array.fold_left 
  (fun (mAcc,k) mk -> 
    let kp1 = (k+1) mod ((er-1)*ec) in 
    let mkp1 = roundArr.(kp1) in
    let mAcc2 = List.fold_left (fun mAcc i -> 
      List.fold_left (fun mAcc j -> 
        try let _ = M.find (i,j) mk in mAcc
        with Not_found -> let potentialNexts = [(i,j,kp1);(i-1,j,kp1);(i+1,j,kp1);(i,j-1,kp1);(i,j+1,kp1)] in
        M3.add (i,j,k) (List.filter (fun (i,j,k) -> if i<1||i>(er-1)||j<1||j>ec then false else try let _ = M.find (i,j) mkp1 in false with Not_found -> true) potentialNexts) mAcc
        ) mAcc width) mAcc height
      in
    let mAcc3 = try let _ = M.find (sr+1,sc) mkp1 in M3.add (sr,sc,k) [(sr,sc,kp1)] mAcc2 with Not_found ->  M3.add (sr,sc,k) [(sr,sc,kp1);(sr+1,sc,kp1)] mAcc2 in 
    let mAcc4 = try let _ = M.find (sr+1,sc) mk in mAcc3 with Not_found -> let lTemp = M3.find (sr+1,sc,k) mAcc3 in M3.add (sr+1,sc,k) ((sr,sc,kp1)::lTemp) mAcc3 in 
    let mAcc5 = try let _ = M.find (er-1,ec) mk in mAcc4 with Not_found -> let lTemp = M3.find (er-1,ec,k) mAcc4 in M3.add (er-1,ec,k) ((er,ec,kp1)::lTemp) mAcc4 in
    try let _ = M.find (er-1,ec) mkp1 in (M3.add (er,ec,k) [(er,ec,kp1)] mAcc5,kp1) with Not_found -> (M3.add (er,ec,k) [(er,ec,kp1);(er-1,ec,kp1)] mAcc5,kp1)
    ) (M3.empty,0) roundArr


let vted = ref(M3.singleton (-1,-1,-1) (-1))
let finalT = ref(-1)
let q = Queue.create ()
let () = List.iter (fun a -> Queue.add (a,0) q) (M3.find (0,1,0) graph)

let () = while not (Queue.is_empty q) do 
  let ((cr,cc,ck),t) = Queue.pop q in  
  if (cr,cc) = (er,ec) then let () = Queue.clear q in finalT := t else
  try let _ = M3.find (cr,cc,ck) !vted in ()
  with Not_found -> let () = vted := M3.add (cr,cc,ck) t !vted in 
  List.iter (fun a -> Queue.add (a,t+1) q) (M3.find (cr,cc,ck) graph) 
done

let () = printf "result part 1: %d\n" (!finalT + 1)
(* result: 264 *)

let () = vted := M3.singleton (-1,-1,-1) (-1)
let finalStatus = ref((-1,-1,-1),-1)
let () = List.iter (fun a -> Queue.add (a,0) q) (M3.find (0,1,0) graph)

let () = while not (Queue.is_empty q) do 
  let ((cr,cc,ck),t) = Queue.pop q in  
  if (cr,cc) = (er,ec) then let () = Queue.clear q in finalStatus := ((cr,cc,ck),t) else
  try let _ = M3.find (cr,cc,ck) !vted in ()
  with Not_found -> let () = vted := M3.add (cr,cc,ck) t !vted in 
  List.iter (fun a -> Queue.add (a,t+1) q) (M3.find (cr,cc,ck) graph) 
done

let () = vted := M3.singleton (-1,-1,-1) (-1)
let (temppos,tempt) = !finalStatus
let () = List.iter (fun a -> Queue.add (a,tempt+1) q) (M3.find temppos graph)

let () = while not (Queue.is_empty q) do 
  let ((cr,cc,ck),t) = Queue.pop q in  
  if (cr,cc) = (sr,sc) then let () = Queue.clear q in finalStatus := ((cr,cc,ck),t) else
  try let _ = M3.find (cr,cc,ck) !vted in ()
  with Not_found -> let () = vted := M3.add (cr,cc,ck) t !vted in 
  List.iter (fun a -> Queue.add (a,t+1) q) (M3.find (cr,cc,ck) graph) 
done

let () = vted := M3.singleton (-1,-1,-1) (-1)
let (temppos,tempt) = !finalStatus
let () = List.iter (fun a -> Queue.add (a,tempt+1) q) (M3.find temppos graph)

let () = while not (Queue.is_empty q) do 
  let ((cr,cc,ck),t) = Queue.pop q in  
  if (cr,cc) = (er,ec) then let () = Queue.clear q in finalStatus := ((cr,cc,ck),t) else
  try let _ = M3.find (cr,cc,ck) !vted in ()
  with Not_found -> let () = vted := M3.add (cr,cc,ck) t !vted in 
  List.iter (fun a -> Queue.add (a,t+1) q) (M3.find (cr,cc,ck) graph) 
done

let (_,tf) = !finalStatus

let () = printf "result part 2: %d\n" (tf+1)
(* result: 789 *)