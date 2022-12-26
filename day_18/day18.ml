open Printf
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

module S = Set.Make(IntPair3)
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic
let l3 = List.map (fun a -> let a::b::c::d = String.split_on_char ',' a in (int_of_string a,int_of_string b,int_of_string c)) l2

let s3 = List.fold_left (fun a b -> S.add b a) S.empty l3

let findSaOf1 (a,b,c) = 
  (try let _ = S.find (a+1,b,c) s3 in 0 with Not_found -> 1) + 
  (try let _ = S.find (a-1,b,c) s3 in 0 with Not_found -> 1) + 
  (try let _ = S.find (a,b+1,c) s3 in 0 with Not_found -> 1) + 
  (try let _ = S.find (a,b-1,c) s3 in 0 with Not_found -> 1) + 
  (try let _ = S.find (a,b,c+1) s3 in 0 with Not_found -> 1) + 
  (try let _ = S.find (a,b,c-1) s3 in 0 with Not_found -> 1)
let totalSA = S.fold (fun a tot -> tot + findSaOf1 a) s3 0
let () = printf "result part 1: %d\n" totalSA
(* answer 3650 *)

(* found max and min to plug in *)
type pos_t = In | Out | Rock | Blank | Temp
let vted = Array.make_matrix 20 20 (Array.make 20 Blank)
let _ = Array.iteri (fun i a -> Array.iteri (fun j b -> vted.(i).(j) <- (Array.make 20 Blank)) a) vted
let () = S.iter (fun (a,b,c) -> vted.(a).(b).(c) <- Rock) s3 (*set all filled as already visited*)
let q = Queue.create ()

let pos_tToS c = 
  (match c with
  | In -> 'I'
  | Out -> 'O'
  | Rock -> 'R'
  | Blank -> 'B'
  | Temp -> 'T')

let rec oneBFS vted currSet inoutref q = 
  try
  let (a,b,c) = Queue.pop q in 
  let () = if a<0||a>19||b<0||b>19||c<0||c>19 then inoutref := Out 
    else if vted.(a).(b).(c) = Blank then
      let () = Queue.add (a+1,b,c) q in
      let () = Queue.add (a-1,b,c) q in
      let () = Queue.add (a,b+1,c) q in
      let () = Queue.add (a,b-1,c) q in
      let () = Queue.add (a,b,c+1) q in
      let () = Queue.add (a,b,c-1) q in 
      let () = vted.(a).(b).(c) <- Temp in
      currSet := S.add (a,b,c) !currSet
    else () in 
  oneBFS vted currSet inoutref q
  with Queue.Empty -> ()

let () = for i = 0 to 19 do 
    for j = 0 to 19 do 
      for k = 0 to 19 do 
        let () = Queue.clear q in
        let () = Queue.add (i,j,k) q in
        let currSet = ref(S.empty) in 
        let inoutref = ref(In) in 
        let () = oneBFS vted currSet inoutref q in
        S.iter (fun (a,b,c) -> vted.(a).(b).(c) <- !inoutref) !currSet
      done
    done
  done

  let findSaOf1_2 (a,b,c) = 
    (try if vted.(a+1).(b).(c) = Out then 1 else 0 with Invalid_argument d -> 1) + 
    (try if vted.(a-1).(b).(c) = Out then 1 else 0 with Invalid_argument d -> 1) + 
    (try if vted.(a).(b+1).(c) = Out then 1 else 0 with Invalid_argument d -> 1) + 
    (try if vted.(a).(b-1).(c) = Out then 1 else 0 with Invalid_argument d -> 1) + 
    (try if vted.(a).(b).(c+1) = Out then 1 else 0 with Invalid_argument d -> 1) + 
    (try if vted.(a).(b).(c-1) = Out then 1 else 0 with Invalid_argument d -> 1)

  let totalSA2 = S.fold (fun a tot -> tot + findSaOf1_2 a) s3 0
  let () = printf "result part 2: %d\n" totalSA2
  (* answer 2118 *)