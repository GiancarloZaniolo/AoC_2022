open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

module S = Set.Make(Int) (* item set *)
module M = Map.Make(Int) (* monkey dict *)
(* monkeys are queues not stacks *)


(* monkey has element set, calc function, test function *)

(* parse input is really bad *)

let toParse = ref(l2)
let lcmVal = ref(1)

let oneInput mDict =
  let (l0::l1::l2::l3::l4::l5::blank::rest) = !toParse in 
  let monkeyNo = int_of_char (String.get l0 7) - 48 in
  let _::_::_::_::items = String.split_on_char ' ' l1 in
  let itemsQ = Queue.create () in
  let () = List.iter (fun a -> Queue.add a itemsQ) (List.map (fun a -> int_of_string (String.sub a 0 2)) items) in 
  let _::_::_::_::_::in1::op1::in2::b = String.split_on_char ' 'l2 in
  let opVal =  
    (match op1 with
    | "+" -> (fun a b -> a + b)
    | "*" -> (fun a b -> a * b)) in
  let newValFun old = (match (in1,in2) with
  | ("old","old") -> opVal old old
  | ("old",a) -> opVal old (int_of_string a)
  | (a,"old") -> opVal old (int_of_string a)
  | (a,b) -> opVal (int_of_string a) (int_of_string b)) in
  let _::_::_::_::_::divBy::b = String.split_on_char ' ' l3 in
  let () = lcmVal := !lcmVal * (int_of_string divBy) in 
  let nextMFun v = if v mod (int_of_string divBy) = 0 then (int_of_char(String.get l4 29) - 48) else (int_of_char(String.get l5 30) - 48) in
  let () = toParse := rest in
  M.add monkeyNo (itemsQ,newValFun,nextMFun,ref(0)) mDict 
    

let rec parseInput mDict = 
  (match !toParse with 
  | [] -> mDict
  | a::b -> parseInput (oneInput mDict))

let mDict = parseInput M.empty

let printDict dict = 
  M.iter (fun a (b,c,d,e) -> 
    let () = printf "Monkey %d items: " a in 
    let () = Queue.iter (fun a -> printf "%d " a) b in
    printf "items seen: %d\n" !e) dict

let rec forOneMonkey mDict (itemsQ,newValFun,nextMFun,itemC) = 
  if Queue.is_empty itemsQ then mDict else 
    let newVal = (newValFun (Queue.pop itemsQ))/3 in
    let newMonkey = nextMFun newVal in 
    let (itemsQ2,newValFun2,nextMFun2,itemD) = M.find newMonkey mDict in
    let () = Queue.add newVal itemsQ2 in
    let () = itemC := !itemC + 1 in
    forOneMonkey (M.add newMonkey (itemsQ2,newValFun2,nextMFun2,itemD) mDict) (itemsQ,newValFun,nextMFun,itemC)

let rec doNIters mDict count = 
  if count = 0 then mDict else
    doNIters (M.fold (fun i a b -> forOneMonkey b a) mDict mDict) (count-1)

let mDict20 = doNIters mDict 20

let mBiz m =
  let ((i1,v1),(i2,v2)) = M.fold (fun i (a,b,c,d) ((e,f),(g,h)) -> 
    if !d > h then 
      if !d > f then
        ((i,!d),(e,f))
      else 
        ((e,f),(i,!d))
    else ((e,f),(g,h))) m ((-1,0),(-1,0)) in
  v1*v2


let () = printf "result part 1: %d\n" (mBiz mDict20)

(* result: 90294 *)


let rec forOneMonkey2 mDict (itemsQ,newValFun,nextMFun,itemC) = 
  if Queue.is_empty itemsQ then mDict else 
    let newVal = (newValFun (Queue.pop itemsQ)) mod !lcmVal in
    let newMonkey = nextMFun newVal in 
    let (itemsQ2,newValFun2,nextMFun2,itemD) = M.find newMonkey mDict in
    let () = Queue.add newVal itemsQ2 in
    let () = itemC := !itemC + 1 in
    forOneMonkey2 (M.add newMonkey (itemsQ2,newValFun2,nextMFun2,itemD) mDict) (itemsQ,newValFun,nextMFun,itemC)


let rec doNIters2 mDict count = 
  if count = 0 then mDict else
    doNIters2 (M.fold (fun i a b -> forOneMonkey2 b a) mDict mDict) (count-1)

let () = toParse := l2
let () = lcmVal := 1
let mDict2 = parseInput M.empty

let mDict10000 = doNIters2 mDict2 10000

let () = printf "result part 2: %d\n" (mBiz mDict10000)

(* result 18170818354 *)