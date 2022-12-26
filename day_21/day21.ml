open Printf
let file = "input.txt"
let ic = open_in file

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []
let l2 = readAll ic

let l3 = List.map (fun a -> String.split_on_char ' ' a) l2 

module M = Map.Make(String)
type exp_t = Val of int | Exp of (string * op * string)
and op = Add | Sub | Mul | Div

exception Weird_input
let rec createAll l m = 
  (match l with 
  | [] -> m
  | (a::b::c::d::e)::f -> let op = (match c with | "+" -> Add | "-" -> Sub | "*" -> Mul | "/" -> Div) in
    createAll f (M.add (String.sub a 0 4) (Exp(b,op,d)) m)
  | (a::b::c)::d -> createAll d (M.add (String.sub a 0 4) (Val (int_of_string b)) m)
  | a::b -> raise Weird_input)

let allExp = createAll l3 M.empty

let rec calcExp m s = 
  (match (M.find s m) with
  | Val v -> v
  | Exp (p,op,n) -> let pv = calcExp m p in let nv = calcExp m n in
    (match op with 
    | Add -> pv + nv 
    | Sub -> pv - nv
    | Mul -> pv * nv 
    | Div -> pv / nv))

let () = printf "result part 1: %d\n" (calcExp allExp "root")
(* result: 152479825094094 *)

type exp2_t = Val2 of int | Exp2 of (exp2_t * op * exp2_t) | Hmn2

let preCalcExp m = 
  let Exp (s1,_,s2) = M.find "root" m in 
  let rec create m s =
    (match (M.find s m) with
    | Val v -> Val2 v
    | Exp ("humn",op,n) -> Exp2(Hmn2,op,create m n)
    | Exp (p,op,"humn") -> Exp2(create m p,op,Hmn2)
    | Exp (p,op,n) -> 
      (match (create m p,create m n) with
      | (Val2 pv,Val2 nv) -> 
          (match op with 
          | Add -> Val2 (pv + nv)
          | Sub -> Val2 (pv - nv)
          | Mul -> Val2 (pv * nv) 
          | Div -> Val2 (pv / nv))
        | (pv,nv) -> (Exp2 (pv,op,nv)))) in
  let s1c = create m s1 in let s2c = create m s2 in 
  (match s1c with
  | Val2 _ -> (s2c,s1c)
  | _ -> (s1c,s2c))
(* human value always on left *)

let rec evalSides (l,r) = 
  let Val2 rv = r in
  (match l with
  | Hmn2 -> rv
  | Val2 _ -> raise Weird_input
  | Exp2 (r,op,l) -> (match (r,op,l) with
    | (x,Add,Val2 y) -> evalSides (x,Val2 (rv-y))
    | (Val2 x,Add,y) -> evalSides (y,Val2 (rv-x))
    | (x,Mul,Val2 y) -> evalSides (x,Val2 (rv/y))
    | (Val2 x,Mul,y) -> evalSides (y,Val2 (rv/x))
    | (x,Sub,Val2 y) -> evalSides (x,Val2 (rv+y))
    | (Val2 x,Sub,y) -> evalSides (y,Val2 (x-rv))
    | (x,Div,Val2 y) -> evalSides (x,Val2 (rv*y))
    | (Val2 x,Div,y) -> evalSides (y,Val2 (x/rv))))

let () = printf "result part 2: %d\n" (evalSides (preCalcExp allExp))
(* result 3360561285172 *)
