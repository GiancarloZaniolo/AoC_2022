(* WARNING: this is not a general solution for part 2, I just plugged in 
   numbers that happened to work for me!!! *)

open Printf
module IntPairs =
  struct 
    type t = int * int
    let compare (a,b) (c,d) = 
      (match Int.compare a c with
      | 0 -> Int.compare b d
      | e -> e)
  end

module S = Set.Make(IntPairs)
let file = "input.txt"
let ic = open_in file
let line = input_line ic
type dir = Left | Right
let rec getChars i s = 
  if i < String.length s then 
    (String.get s i)::(getChars (i+1) s)
else []
let typed = List.map (fun a -> if a = '<' then Left else Right) (getChars 0 line)

let inQ = Queue.create ()
let refill_inQ () = List.iter (fun a -> Queue.add a inQ) typed


let sh1 =  [(0,0);(0,1);(0,2);(0,3)]
let sh2 =  [(0,1);(1,0);(1,1);(1,2);(2,1)]
let sh3 = [(0,0);(0,1);(0,2);(1,2);(2,2)]
let sh4 =  [(0,0);(1,0);(2,0);(3,0)]
let sh5 =  [(0,0);(0,1);(1,0);(1,1)]
let allShList = [sh1;sh2;sh3;sh4;sh5]
let shQ = Queue.create ()
let refill_shQ () = List.iter (fun a -> Queue.add a shQ) allShList

let currH = ref(-1)
let allPlaced = ref(S.empty)
let lBound = 7
let rec oneShape () = 
  let addElemToSet r c shLst = List.iter (fun (a,b) -> let () = allPlaced := S.add (r+a,c+b) !allPlaced in currH := Int.max !currH (r+a)) shLst in
  let rec sideMv row col shLst = let () = if Queue.is_empty inQ then refill_inQ () in
    (match (Queue.pop inQ) with
    | Left -> let nisMv = List.fold_left (fun a (b,c) -> (a || ((c+col) <= 0 || (try S.find (b+row,c+col-1) (!allPlaced) = (b+row,c+col-1) with Not_found -> false)))) false shLst in
      if nisMv then downMv row col shLst else downMv row (col-1) shLst
    | Right -> let nisMv = List.fold_left (fun a (b,c) -> (a || ((c+col) >= (lBound-1) || (try S.find (b+row,c+col+1) (!allPlaced) = (b+row,c+col+1) with Not_found -> false)))) false shLst in
      if nisMv then downMv row col shLst else downMv row (col+1) shLst)
  and downMv row col shLst = 
    let nisMv = List.fold_left (fun a (b,c) -> (a || ((b+row) <= 0 || (try S.find (b+row-1,c+col) (!allPlaced) = (b+row-1,c+col) with Not_found -> false)))) false shLst in
    if nisMv then  addElemToSet row col shLst else sideMv (row-1) col shLst in
  let () = if Queue.is_empty shQ then refill_shQ () in
  sideMv (!currH + 4) 2 (Queue.pop shQ)

let () = for i = 1 to 2022 do oneShape () done
let () = printf "result part 1: %d\n" (!currH + 1)

let prevH = ref(-1)
let blCount = ref(0)
let blCountPrev = ref(-1)
let rec oneShape2 () = 
  let addElemToSet r c shLst = List.iter (fun (a,b) -> let () = allPlaced := S.add (r+a,c+b) !allPlaced in currH := Int.max !currH (r+a)) shLst in
  let rec sideMv row col shLst = 
    let () = if Queue.is_empty inQ then 
      let () = prevH := !currH in
      let () = blCountPrev := !blCount in
      refill_inQ () in
    (match (Queue.pop inQ) with
    | Left -> let nisMv = List.fold_left (fun a (b,c) -> (a || ((c+col) <= 0 || (try S.find (b+row,c+col-1) (!allPlaced) = (b+row,c+col-1) with Not_found -> false)))) false shLst in
      if nisMv then downMv row col shLst else downMv row (col-1) shLst
    | Right -> let nisMv = List.fold_left (fun a (b,c) -> (a || ((c+col) >= (lBound-1) || (try S.find (b+row,c+col+1) (!allPlaced) = (b+row,c+col+1) with Not_found -> false)))) false shLst in
      if nisMv then downMv row col shLst else downMv row (col+1) shLst)
  and downMv row col shLst = 
    let nisMv = List.fold_left (fun a (b,c) -> (a || ((b+row) <= 0 || (try S.find (b+row-1,c+col) (!allPlaced) = (b+row-1,c+col) with Not_found -> false)))) false shLst in
    if nisMv then  addElemToSet row col shLst else sideMv (row-1) col shLst in
  let () = if Queue.is_empty shQ then refill_shQ () in
  sideMv (!currH + 4) 2 (Queue.pop shQ)

let () = while not (Queue.is_empty inQ) do let _ = Queue.pop inQ in () done
let () = while not (Queue.is_empty shQ) do let _ = Queue.pop shQ in () done
let () = currH := -1
let () = allPlaced := S.empty

(* I am committed to doing this the jank way *)

let numB4Repeats = 1736
let () = for i = 1 to numB4Repeats do oneShape2 () done
let heighB4Repeats = !currH + 1 (* "proved" correct *)
let blSizeOfRepeat = 1720
let () = for i = 1 to blSizeOfRepeat do oneShape2 () done
let heightOfRepeat = !currH + 1 - heighB4Repeats (* "proved" correct *)
let numOfRepeats = ((1000000000000 - numB4Repeats) / blSizeOfRepeat)
let numOfFinal = ((1000000000000 - numB4Repeats) mod blSizeOfRepeat)
let () = for i = 1 to numOfFinal do oneShape2 () done
let heightOfFinal = !currH + 1 - heightOfRepeat - heighB4Repeats
let total = heighB4Repeats + heightOfFinal + (numOfRepeats * heightOfRepeat)


let () = printf "result part 2: %d\n" total

(* 1572093023267 correct answer *)

