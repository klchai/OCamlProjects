(** I Permutations *)
(* Ex 1 *)
let rec insert x l = 
    match l with
    | [] -> [[x]]
    | head::tail -> (x::l)::(List.map (fun lst -> head::lst) (insert x tail))
;;

(* 以下是其他的写法
let ins_all_positions x l =
    let rec aux l1 l2 =
        match l1 with
        | [] -> [l2 @ [x]]
        | y::s -> (l2 @ [x] @ l1) :: aux s (l2 @ [y])
    in
aux l []
;;

(* note that in order to preserve certain order and also show the conciseness of the implementation, no tail-recursive is used *)
let ins_all_positions x l =  
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
  in
  aux [] [] l
;;
*)

(* Ex 2 *)
let rec permutations l = 
    match l with
    | [] -> [[]]
    | x::[] -> [[x]]
    | head::tail -> List.fold_left(fun acc l -> (insert head l)@acc) [] (permutations tail)
;;

(* 以下是其他写法
let rec permutations lst = 
  match lst with
  | hd::tl -> List.concat (List.map (insert hd) (permutations tl))
  | _ -> [lst]
;;

let rec permutation = function  
  | [] -> []
  | x::[] -> [[x]] (* we must specify this edge case *)
  | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p ) [] (permutation xs)
*)

(** II Arbres binaires *)
