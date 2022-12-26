open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

type outer = End | Cons of lElem * outer
and lElem = L of outer | V of int

let rec list_from_string s = 
  match s with
  | "" -> []
  | _ -> (String.get s 0)::(list_from_string (String.sub s 1 (String.length s - 1))) (* cursed *)

let rec readOneLine s = 
  let l = ref(list_from_string s) in
  let rec readRec () = 
    (match !l with
    | [] -> End
    | a::b -> let () = l := b in (match a with
      | '[' -> let e = readRec () in Cons (L(e),readRec ())
      | ']' -> End
      | ',' -> readRec ()
      | '1' -> let (c::d) = b in 
        if c = '0' then let () = l := d in Cons (V(10),readRec ()) else Cons (V(1),readRec ())
      |  a ->  Cons (V((int_of_char a)-48),readRec ()))) in
  readRec ()

let rec parseAll l =
  (match l with
  | [] -> []
  | a::b::c::d -> ((readOneLine a),(readOneLine b))::(parseAll d)
  | a::b::c -> [((readOneLine a),(readOneLine b))])

let rec printOneLine l = 
  (match l with 
  | End -> printf "]"
  | Cons (a,b) -> let () = (match a with
    | V c -> printf "%d " c
    | L c -> let () = printf "[" in printOneLine c) in printOneLine b)

let rec printAll l =
  (match l with
  | [] -> ()
  | (a,b)::c -> let () = printOneLine a in let () = printf "\n" in let () = printOneLine b in let () = printf "\n\n" in printAll c) 

let parsed = parseAll l2

type listcmp = LEFT | EQUAL | RIGHT
exception Lists_are_equal
let rec compareOne (a,b) = 
  let rec compareInner (a,b) = 
    (match (a,b) with
    | (V(c),V(d)) -> if c < d then LEFT else if c > d then RIGHT else EQUAL
    | (V(c),L(d)) -> compareOne (Cons(a,End),d)
    | (L(c),V(d)) -> compareOne (c,Cons(b,End))
    | (L(c),L(d)) -> compareOne (c,d))
  in
  (match (a,b) with
  | (End,End) -> EQUAL
  | (Cons(c,d),End) -> RIGHT
  | (End,Cons(c,d)) -> LEFT
  | (Cons(c,d),Cons(e,f)) -> 
    (match compareInner (c,e) with
    | LEFT -> LEFT 
    | RIGHT -> RIGHT
    | EQUAL -> compareOne (d,f)))

let rec printCompareAll l =
  (match l with
  | [] -> ()
  | a::b -> let () = printf "res: %s\n" (match (compareOne a)with 
    | LEFT -> "in"
    | RIGHT -> "not"
    | EQUAL -> "equal") in printCompareAll b) 

let rec countCompareAll l i = 
  (match l with
  | [] -> 0
  | a::b -> (match (compareOne a)with 
    | LEFT -> i + countCompareAll b (i+1)
    | RIGHT -> countCompareAll b (i+1)
    | EQUAL -> raise Lists_are_equal))

let () = printf "result part 1: %d\n" (countCompareAll parsed 1)

let rec parseAll2 l =
  (match l with
  | [] -> []
  | a::b::c::d -> (readOneLine a)::(readOneLine b)::(parseAll2 d)
  | a::b::c -> (readOneLine a)::(readOneLine b)::[])

let rec printAll2 l =
  (match l with
  | [] -> ()
  | a::b -> let () = printOneLine a in let () = printf "\n" in printAll2 b) 

let parsed2 = parseAll2 l2

let outerCompare a b = 
  (match (compareOne (a,b)) with
  | LEFT -> -1
  | RIGHT -> 1
  | EQUAL -> 0)

let rec outerInsert l elem = 
  (match l with
  | [] -> elem::[]
  | a::b -> if (outerCompare elem a) < 0 then elem::l else a::(outerInsert b elem))

exception Elem_not_found
let rec findElem l elem = 
  (match l with
  | [] -> raise Elem_not_found
  | a::b -> if (outerCompare elem a) = 0 then 1 else 1 + (findElem b elem ))

let sorted = List.sort outerCompare parsed2
let insert1 = outerInsert sorted (Cons(L(Cons(L(Cons(V(2),End)),End)),End))
let insert2 = outerInsert insert1 (Cons(L(Cons(L(Cons(V(6),End)),End)),End))

let pos1 = findElem insert2 (Cons(L(Cons(L(Cons(V(2),End)),End)),End))
let pos2 = findElem insert2 (Cons(L(Cons(L(Cons(V(6),End)),End)),End))

let () = printf "result part 2: %d\n" (pos1 * pos2)