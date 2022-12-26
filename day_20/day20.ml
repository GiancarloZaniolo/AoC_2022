open Printf
open Option
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic
let l3 = List.map (fun a -> int_of_string a) l2

type node = {mutable prev : node option; n:int ; mutable next : node option}

let l4 = List.map (fun a -> {prev = none; n = a; next = none}) l3
let _ = List.fold_left (fun n r -> let () = r.prev <- Some n in r) (List.nth l4 (List.length l4 - 1)) l4 
let _ = List.fold_right (fun r n -> let () = r.next <- Some n in r) l4 (List.nth l4 0)

let l5 = List.map (fun a -> ref a) l4
let zeroElem = List.find (fun a -> (!a).n = 0) l5

let insertOne node_r = 
  if (!node_r).n = 0 then () else
  let Some prev = (!node_r).prev in 
  let Some next = (!node_r).next in 
  let () = prev.next <- Some next in 
  let () = next.prev <- Some prev in 
  let rec iterCnt_N c v = if c < 1 then (*off-by-1 correction*)
    let Some p = v.prev in iterCnt_N (c+1) p else v in
  let rec iterCnt c v = if c > 0 then 
    let Some n = v.next in iterCnt (c-1) n else v in
  let new_prev = if (!node_r).n>=0 then iterCnt (!node_r).n (!node_r) 
    else iterCnt_N (!node_r).n (!node_r) in 
  let Some new_next = new_prev.next in 
  let () = node_r := {prev=Some new_prev;n=((!node_r).n);next=Some new_next} in 
  let () = new_prev.next <- Some (!node_r) in 
  new_next.prev <- Some (!node_r)

let rec printll node cnt =
  if cnt > 0 then
  let () = printf "%d " node.n in 
  let Some next = node.next in printll next (cnt-1)
  else ()

let () = List.iter (fun a -> insertOne a) l5

let rec findNth node cnt = 
  if cnt > 0 then
    let Some next = node.next in findNth next (cnt-1)
  else node
let node1k = findNth !zeroElem 1000
let node2k = findNth !zeroElem 2000
let node3k = findNth !zeroElem 3000

let () = printf "result part 1: %d\n%!" (node1k.n+node2k.n+node3k.n)
(* result: 4267 *)

(* decryption key: 811589153 *)
let llen = List.length l5
let insertOne_2 node_r = 
  if (!node_r).n = 0 then () else
  let Some prev = (!node_r).prev in 
  let Some next = (!node_r).next in 
  let () = prev.next <- Some next in 
  let () = next.prev <- Some prev in 
  let rec iterCnt_N c v = if c < 1 then (*off-by-1 correction*)
    let Some p = v.prev in iterCnt_N (c+1) p else v in
  let rec iterCnt c v = if c > 0 then 
    let Some n = v.next in iterCnt (c-1) n else v in
(* let () = printf "n mod llen: %d\n" (((!node_r).n)mod llen) in *)
  let new_prev = if (!node_r).n>=0 then iterCnt ((((!node_r).n)mod (llen-1))) (!node_r) 
    else iterCnt_N ((((!node_r).n)mod (llen-1))) (!node_r) in 
  let Some new_next = new_prev.next in 
  let () = node_r := {prev=Some new_prev;n=((!node_r).n);next=Some new_next} in 
  let () = new_prev.next <- Some (!node_r) in 
  new_next.prev <- Some (!node_r)

  let l6 = List.map (fun a -> {prev = none; n = (a * 811589153); next = none}) l3
  let _ = List.fold_left (fun n r -> let () = r.prev <- Some n in r) (List.nth l6 (List.length l6 - 1)) l6 
  let _ = List.fold_right (fun r n -> let () = r.next <- Some n in r) l6 (List.nth l6 0)
  let l7 = List.map (fun a -> ref a) l6
  let zeroElem_2 = List.find (fun a -> (!a).n = 0) l7

let () = for i = 1 to 10 do 
  List.iter (fun a -> insertOne_2 a) l7
done 

let node1k_2 = findNth !zeroElem_2 1000
let node2k_2 = findNth !zeroElem_2 2000
let node3k_2 = findNth !zeroElem_2 3000

let () = printf "result part 2: %d\n%!" (node1k_2.n+node2k_2.n+node3k_2.n)
(* result: 6871725358451 *)
