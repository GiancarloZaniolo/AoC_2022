open Printf

let file = "input.txt"

exception Sad

let ic = open_in file
let l = []

let rec readAll ic l = 
  let line = try input_line ic
    with e -> let () = close_in_noerr ic in "!" in
    if line = "!" then
      l 
    else readAll ic (line::l)

let l2 = readAll ic l

let getPoints s =
  match s with
  | "A X" -> 1 + 3
  | "A Y" -> 2 + 6
  | "A Z" -> 3 + 0
  | "B X" -> 1 + 0
  | "B Y" -> 2 + 3
  | "B Z" -> 3 + 6
  | "C X" -> 1 + 6
  | "C Y" -> 2 + 0
  | "C Z" -> 3 + 3
  | _ -> -9999

let points = List.map getPoints l2

let a = List.fold_left (fun a b -> a + b) 0 points

let () = printf "result 1: %d\n" a
(* result 1: 9759 *)


let getNewStr s =
  match s with
  | "A X" -> "A Z"
  | "A Y" -> "A X"
  | "A Z" -> "A Y"
  | "B X" -> "B X"
  | "B Y" -> "B Y"
  | "B Z" -> "B Z"
  | "C X" -> "C Y"
  | "C Y" -> "C Z"
  | "C Z" -> "C X"
  | _ -> "aaaaaaaa"

let points2 = List.map getNewStr l2
let points22 = List.map getPoints points2

let b = List.fold_left (+) 0 points22

let () = printf "result 2: %d\n" b
(* result 2: 12429 *)