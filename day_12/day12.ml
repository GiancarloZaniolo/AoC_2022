open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

let dist_mat = Array.make_matrix (List.length l2) (String.length (List.nth l2 0)) Int.max_int

let char_mat = Array.make_matrix (List.length l2) (String.length (List.nth l2 0)) '\x00'
let rec fillCharMat l i = 
  (match l with
  | [] -> ()
  | a::b -> let _ = String.fold_left (fun j c -> let () = char_mat.(i).(j) <- c in (j+1)) 0 a in 
  fillCharMat b (i+1))

let () = fillCharMat l2 0

let rec findChar a c = 
  let (_,(x,y)) = Array.fold_left 
  (fun (i,(k,l)) e -> let (_,(x,y)) = Array.fold_left 
    (fun (j,(k,l)) e -> if e = c then ((j+1),(i,j)) else ((j+1),(k,l))) (0,(k,l)) e in ((i+1),(x,y))) (0,(-1,-1)) a
in (x,y)

let (sr,sc) = findChar char_mat 'S'
let (er,ec) = findChar char_mat 'E'
let () = char_mat.(sr).(sc) <- 'a'
let () = char_mat.(er).(ec) <- 'z'

let v_q = Queue.create ()
let e_from_v (a,b) q =
  let () = if a > 0 then 
    if (int_of_char(char_mat.(a-1).(b))) - (int_of_char(char_mat.(a).(b))) <= 1 then Queue.add ((a,b),((a-1),b)) q else () else () in
  let () = if a < (Array.length char_mat) - 1 then
    if (int_of_char(char_mat.(a+1).(b))) - (int_of_char(char_mat.(a).(b))) <= 1 then Queue.add ((a,b),((a+1),b)) q else () else () in
  let () = if b > 0 then 
    if (int_of_char(char_mat.(a).(b-1))) - (int_of_char(char_mat.(a).(b))) <= 1 then Queue.add ((a,b),(a,(b-1))) q else () else () in
  if b < (Array.length char_mat.(0)) - 1 then
    if (int_of_char(char_mat.(a).(b+1))) - (int_of_char(char_mat.(a).(b))) <= 1 then Queue.add ((a,b),(a,(b+1))) q else () else ()

let () = dist_mat.(sr).(sc) <- 0
let () = e_from_v (sr,sc) v_q

let rec big_bfs q = 
    try let ((v,w),(x,y)) = Queue.pop q in
    let () = if dist_mat.(x).(y) = Int.max_int then 
      let () = dist_mat.(x).(y) <- (dist_mat.(v).(w) + 1) in 
        e_from_v (x,y) q else ()
    in big_bfs q
with Queue.Empty -> ()

let () = big_bfs v_q

let () = let a = dist_mat.(er).(ec) in printf "result part 1: %d\n" a

let (_,final) = Array.fold_left 
  (fun (i,best) a -> 
    let (_,best) = Array.fold_left 
      (fun (j,best) a -> 
        if char_mat.(i).(j) = 'a' then
          let _ = Array.fold_left (fun i a -> let _ = Array.fold_left (fun j b -> let () = dist_mat.(i).(j) <- Int.max_int in (j+1)) 0 a in (i+1)) 0 dist_mat in
          let () = Queue.clear v_q in
          let () = dist_mat.(i).(j) <- 0 in
          let () = e_from_v (i,j) v_q in
          let () = big_bfs v_q in
          if (dist_mat.(er).(ec)) < best then (j+1,dist_mat.(er).(ec)) else (j+1,best) 
        else (j+1,best) )
        (0,best) a in ((i+1),best)) (0,Int.max_int) dist_mat

let () = printf "result part 2: %d\n" final