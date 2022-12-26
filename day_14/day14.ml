open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic
let l3 = List.map (fun a -> String.split_on_char ' ' a) l2
let rec remArrows l = (match l with | [] -> [] | a::b::c -> a::(remArrows c) | a::b -> a::(remArrows b))
let l4 = List.map remArrows l3 
let l5 = List.map (fun a -> List.flatten (List.map (fun b -> String.split_on_char ',' b) a)) l4
let rec parseAll l = 
  let rec parseOne l = 
    (match l with
    | [] -> []
    | a::b::c -> (int_of_string a,int_of_string b)::(parseOne c)) in
  (match l with
  | [] -> []
  | a::b -> (parseOne a)::(parseAll b))
let l6 = parseAll l5

let rec printparse l = 
  let rec printOne l = 
    (match l with
    | [] -> ()
    | (a,b)::c -> let () = printf "(%d,%d)->" a b in printOne c)
  in (match l with
  | [] -> ()
  | a::b -> let () = printOne a in let () = printf "\n" in printparse b)

let (max1,max2) = List.fold_left (fun (a,b) l -> List.fold_left (fun (a,b) (c,d) -> (Int.max a c,Int.max b d)) (a,b) l) (0,0) l6

type elem = Air | Rock | Sand
let m = Array.make_matrix (max2+1) (max1+1) Air

let rec initSand l =
  let rec oneInstr (prevx,prevy) l = 
  (match l with
  | [] -> m.(prevy).(prevx) <- Rock
  | (ax,ay)::b -> 
    let () = if (prevx,prevy) = (-1,-1) then oneInstr (ax,ay) b else
      let rec placeRocks (sx,sy) (ex,ey) =
        if (sx,sy) = (ex,ey) then () else let () = m.(sy).(sx) <- Rock in placeRocks (sx + (try ((ex-sx)/(Int.abs(ex-sx))) with e -> 0),sy + (try ((ey-sy)/(Int.abs(ey-sy))) with e -> 0)) (ex,ey) in
      placeRocks (prevx,prevy) (ax,ay)
    in oneInstr (ax,ay) b)
in
  (match l with
  | [] -> ()
  | a::b -> let () = oneInstr (-1,-1) a in initSand b) 

let () = initSand l6
let sandCount = ref(0)

let rec dropAll () = 
  let rec dropOne (x,y) = 
    if (m.(y+1).(x)) = Air then dropOne (x,y+1) else
    if (m.(y+1).(x-1)) = Air then dropOne (x-1,y+1) else
    if (m.(y+1).(x+1)) = Air then dropOne (x+1,y+1) else m.(y).(x) <- Sand
  in try let () = dropOne (500,0) in let () = sandCount := !sandCount + 1 in dropAll () with e -> ()

let () = dropAll ()

(* for print image to file *)
(* let () = Array.iter (fun a -> let () = Array.iter (fun b -> printf "%c" (match b with Air -> '.' | Rock -> '#' | Sand -> 'o')) a in printf "\n") m *)

let () = printf "result part 1: %d\n" !sandCount

(* result 888 *)

let m2 = Array.make_matrix (max2+3) ((max1+1) * 2) Air (* jank *)

let rec initSand2 l =
  let rec oneInstr (prevx,prevy) l = 
  (match l with
  | [] -> m2.(prevy).(prevx) <- Rock
  | (ax,ay)::b -> 
    let () = if (prevx,prevy) = (-1,-1) then oneInstr (ax,ay) b else
      let rec placeRocks (sx,sy) (ex,ey) =
        if (sx,sy) = (ex,ey) then () else let () = m2.(sy).(sx) <- Rock in placeRocks (sx + (try ((ex-sx)/(Int.abs(ex-sx))) with e -> 0),sy + (try ((ey-sy)/(Int.abs(ey-sy))) with e -> 0)) (ex,ey) in
      placeRocks (prevx,prevy) (ax,ay)
    in oneInstr (ax,ay) b)
in
  let _ = Array.fold_left (fun a e -> let _ = m2.(max2+2).(a) <- Rock in (a+1)) 0 (Array.get m2 0) in
  (match l with
  | [] -> ()
  | a::b -> let () = oneInstr (-1,-1) a in initSand2 b) 

let () = initSand2 l6

let sandCount2 = ref(0)

let rec dropAll2 () = 
  let rec dropOne (x,y) = 
    if (m2.(y).(x)) = Sand then raise End_of_file else (* extremely sus*)
    if (m2.(y+1).(x)) = Air then dropOne (x,y+1) else
    if (m2.(y+1).(x-1)) = Air then dropOne (x-1,y+1) else
    if (m2.(y+1).(x+1)) = Air then dropOne (x+1,y+1) else m2.(y).(x) <- Sand
  in try let () = dropOne (500,0) in let () = sandCount2 := !sandCount2 + 1 in dropAll2 () with e -> ()

let () = dropAll2 ()

(* for print image to file *)
(* let () = Array.iter (fun a -> let () = Array.iter (fun b -> printf "%c" (match b with Air -> '.' | Rock -> '#' | Sand -> 'o')) a in printf "\n") m2 *)

let () = printf "result part 2: %d\n" !sandCount2

(* result 26461 *)
