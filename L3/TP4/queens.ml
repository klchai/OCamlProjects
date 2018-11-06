let list_of_int n =
  let rec loi acc n = 
  if n <= 0 then acc
  else loi (n::acc) (n-1)
  in
  loi [] n

let succl = List.map succ
let predl = List.map pred

let diff a b =
  List.fold_left (fun acc x -> if List.mem x b then acc else x::acc) [] a

let remove a x =
  List.fold_left (fun acc y -> if y = x then acc else y :: acc) [] a

let rec queens a b c = 
  if a=[] then 1
  else 
    let e = diff (diff a b) c in
    List.fold_left 
      (fun acc d -> 
	 acc + queens (remove a d) (succl (d::b)) (predl (d::c))) 0 e
      

let queens n = queens (list_of_int n) [] []
  
let () =
  let n = int_of_string Sys.argv.(1) in
  Printf.printf "Le problème des %d reines à %d solutions \n" n (queens n)
