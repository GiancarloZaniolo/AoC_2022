open Printf
let file = "input.txt"
let ic = open_in file


let int_of_bool b = if b then 1 else 0
let size = 99 - 1
let size2 = 99

let rec readAll ic = 
  try let line = input_line ic in
  line::(readAll ic)
  with End_of_file -> []

let l2 = readAll ic

let trees = Array.make_matrix size2 size2 0 


let rec readTrees l i arr = 
  let rec readRow s a j = 
    (match j with
    | 99 -> ()
    | _ -> let () = Array.set a j (int_of_char(String.get s j) - 48) in readRow s a (j+1)) in
  (match l with
  | [] -> ()
  | a::b -> let () = readRow a (Array.get arr i) 0 in readTrees b (i+1) arr)

let () = readTrees l2 0 trees

let isVis = Array.make_matrix size2 size2 false

let _ = for i = 0 to Array.length trees - 1 do
  let a = ref(-1) in 
  for j = 0 to Array.length (trees.(i)) - 1 do
    if trees.(i).(j) > !a then let () = isVis.(i).(j) <- true in a := trees.(i).(j)
  done
done

let _ = for i = 0 to Array.length trees - 1 do
  let a = ref(-1) in 
  for j = 0 to Array.length (trees.(i)) - 1 do
    if trees.(j).(i) > !a then let () = isVis.(j).(i) <- true in a := trees.(j).(i)
  done
done

  let _ = for i = 0 to Array.length trees - 1 do
    let a = ref(-1) in 
    for j = 0 to Array.length (trees.(i)) - 1 do
      if trees.(i).(size-j) > !a then let () = isVis.(i).(size-j) <- true in a := trees.(i).(size-j)
    done
  done

let _ = for i = 0 to Array.length trees - 1 do
  let a = ref(-1) in 
  for j = 0 to Array.length (trees.(i)) - 1 do
    if trees.(size-j).(i) > !a then let () = isVis.(size-j).(i) <- true in a := trees.(size-j).(i)
  done
done

let numOfVis = ref(0)
let _ = for i = 0 to Array.length trees - 1 do
  for j = 0 to Array.length (trees.(i)) - 1 do
    if isVis.(i).(j) = true then numOfVis := !numOfVis + 1
  done
done

let () = printf "numOfVis:%d\n" !numOfVis

let findScoreUp i j = 
  let rec inner k l s = 
    if k < 0 || k > size || l < 0 || l > size then s else if (trees.(i).(j)) <= (trees.(k).(l)) then s + 1
    else inner (k+1) l (s+1) in 
  inner (i+1) j 0
let findScoreDown i j = 
  let rec inner k l s = 
    if k < 0 || k > size || l < 0 || l > size then s else if (trees.(i).(j)) <= (trees.(k).(l)) then s + 1
    else inner (k-1) l (s+1) in 
  inner (i-1) j 0
let findScoreRight i j = 
  let rec inner k l s = 
    if k < 0 || k > size || l < 0 || l > size then s else if (trees.(i).(j)) <= (trees.(k).(l)) then s + 1
    else inner k (l+1) (s+1) in 
  inner i (j+1) 0
let findScoreLeft i j = 
  let rec inner k l s = 
    if k < 0 || k > size || l < 0 || l > size then s else if (trees.(i).(j)) <= (trees.(k).(l)) then s + 1
    else inner k (l-1) (s+1) in 
  inner i (j-1) 0


let findAllScores i j = 
  (findScoreLeft i j) * (findScoreRight i j) * (findScoreUp i j) * (findScoreDown i j)

let sScores = Array.make_matrix size2 size2 0


let _ = for i = 0 to Array.length trees - 1 do 
  for j = 0 to Array.length (trees.(1)) - 1 do 
    Array.set (Array.get sScores i) j (findAllScores i j)
  done
done

let max_val = Array.fold_left 
  (fun b a -> Int.max b (Array.fold_left (fun d c -> Int.max d c) (-1) a)) (-1) sScores

let () = printf "max_val %d\n" max_val

(* val 345744 *)


