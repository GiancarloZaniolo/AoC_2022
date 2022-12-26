open Printf
let file = "input.txt"
let ic = open_in file
let ic2 = Scanf.Scanning.from_channel ic
exception Bad_input
let rec readAll ic = 
  try
  let chunk = try Scanf.bscanf ic "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.\n" (fun a b c d e f g -> (b,c,d,e,f,g)) with 
      | Scanf.Scan_failure s -> let _ = printf "%s" s in raise Bad_input
      | End_of_file -> raise End_of_file in 
    chunk::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic2

let zeroOrPos a = if a >= 0 then a else 0
let bestGeo = ref(0)
let roundUpDiv a b = (a + b - 1) / b
let rec forOneInput currR (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) orCnt clCnt obCnt gCnt orRCnt clRCnt obRCnt gRCnt = 
if 
  (let roundsNec = (zeroOrPos (roundUpDiv(orRCostOr-orCnt) orRCnt))+1 in (*must check if over*)
  if currR+roundsNec>24 then 0 else 
  let () = forOneInput (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-orRCostOr) (clCnt+(clRCnt*roundsNec)) (obCnt+(obRCnt*roundsNec)) (gCnt+(gRCnt*roundsNec)) (orRCnt+1) clRCnt obRCnt gRCnt
  in 1) 
+ 
  (let roundsNec = (zeroOrPos (roundUpDiv (clRCostOr-orCnt) orRCnt))+1 in (*must check if over*)
    if currR+roundsNec>24 then 0 else 
    let () = forOneInput (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-clRCostOr) (clCnt+(clRCnt*roundsNec)) (obCnt+(obRCnt*roundsNec)) (gCnt+(gRCnt*roundsNec)) orRCnt (clRCnt+1) obRCnt gRCnt
    in 1)
+ 
  (if clRCnt = 0 then 0 else
  let roundsNec = (Int.max (zeroOrPos (roundUpDiv (obRCostOr-orCnt) orRCnt)) (zeroOrPos (roundUpDiv (obRCostCl-clCnt) clRCnt)))+1 in 
  if currR+roundsNec>24 then 0 else
  let () = forOneInput (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-obRCostOr) (clCnt+(clRCnt*roundsNec)-obRCostCl) (obCnt+(obRCnt*roundsNec)) (gCnt+(gRCnt*roundsNec)) orRCnt clRCnt (obRCnt+1) gRCnt
  in 1)
+ 
  (if obRCnt = 0 then 0 else
  let roundsNec = (Int.max (zeroOrPos (roundUpDiv (gRCostOr-orCnt) orRCnt)) (zeroOrPos (roundUpDiv (gRCostOb-obCnt) obRCnt)))+1 in 
  if currR+roundsNec>24 then 0 else
  let () = forOneInput (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-gRCostOr) (clCnt+(clRCnt*roundsNec)) (obCnt+(obRCnt*roundsNec)-gRCostOb) (gCnt+(gRCnt*roundsNec)) orRCnt clRCnt obRCnt (gRCnt+1)
  in 1)
 = 0 then 
  let totalGeo = gCnt + ((24-currR)*gRCnt) in 
  if totalGeo > !bestGeo then 
    bestGeo := totalGeo 
  else ()
else ()
 
let rec forAllInput l i total =
  (match l with
  | [] -> total
  | a::b -> 
    let () = bestGeo := 0 in
    let () = forOneInput 0 a 0 0 0 0 1 0 0 0 in 
    forAllInput b (i+1) (total+(i*(!bestGeo))))

let () = printf "result 1: %d\n" (forAllInput l2 1 0)

(* result 1466 *)

let bestGeo2 = ref(0)
let rec forOneInput2 currR (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) orCnt clCnt obCnt gCnt orRCnt clRCnt obRCnt gRCnt = 
if 
  (if orRCnt >= Int.max (Int.max orRCostOr clRCostOr) (Int.max obRCostOr gRCostOr) then 0 else
  let roundsNec = (zeroOrPos (roundUpDiv(orRCostOr-orCnt) orRCnt))+1 in (*must check if over*)
  if currR+roundsNec>32 then 0 else 
  let () = forOneInput2 (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-orRCostOr) (clCnt+(clRCnt*roundsNec)) (obCnt+(obRCnt*roundsNec)) (gCnt+(gRCnt*roundsNec)) (orRCnt+1) clRCnt obRCnt gRCnt
  in 1) 
+ 
  (if clRCnt >= obRCostCl then 0 else
  let roundsNec = (zeroOrPos (roundUpDiv (clRCostOr-orCnt) orRCnt))+1 in (*must check if over*)
  if currR+roundsNec>32 then 0 else 
  let () = forOneInput2 (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-clRCostOr) (clCnt+(clRCnt*roundsNec)) (obCnt+(obRCnt*roundsNec)) (gCnt+(gRCnt*roundsNec)) orRCnt (clRCnt+1) obRCnt gRCnt
  in 1)
+ 
  (if clRCnt = 0 || obRCnt >= gRCostOb then 0 else
  let roundsNec = (Int.max (zeroOrPos (roundUpDiv (obRCostOr-orCnt) orRCnt)) (zeroOrPos (roundUpDiv (obRCostCl-clCnt) clRCnt)))+1 in 
  if currR+roundsNec>32 then 0 else
  let () = forOneInput2 (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-obRCostOr) (clCnt+(clRCnt*roundsNec)-obRCostCl) (obCnt+(obRCnt*roundsNec)) (gCnt+(gRCnt*roundsNec)) orRCnt clRCnt (obRCnt+1) gRCnt
  in 1)
+ 
  (if obRCnt = 0 then 0 else
  let roundsNec = (Int.max (zeroOrPos (roundUpDiv (gRCostOr-orCnt) orRCnt)) (zeroOrPos (roundUpDiv (gRCostOb-obCnt) obRCnt)))+1 in 
  if currR+roundsNec>32 then 0 else
  let () = forOneInput2 (currR+roundsNec) (orRCostOr,clRCostOr,obRCostOr,obRCostCl,gRCostOr,gRCostOb) (orCnt+(orRCnt*roundsNec)-gRCostOr) (clCnt+(clRCnt*roundsNec)) (obCnt+(obRCnt*roundsNec)-gRCostOb) (gCnt+(gRCnt*roundsNec)) orRCnt clRCnt obRCnt (gRCnt+1)
  in 1)
 = 0 then 

  let totalGeo = gCnt + ((32-currR)*gRCnt) in 
  if totalGeo > !bestGeo2 then 
    bestGeo2 := totalGeo 
  else ()
else ()
 
let rec forAllInput2 l i total =
  if i > 3 then total else
  (match l with
  | [] -> total
  | a::b -> 
    let () = bestGeo2 := 0 in
    let () = forOneInput2 0 a 0 0 0 0 1 0 0 0 in 
    forAllInput2 b (i+1) (total*(!bestGeo2)))

let () = printf "result 2: %d\n" (forAllInput2 l2 1 1)
(* result 8250 *)