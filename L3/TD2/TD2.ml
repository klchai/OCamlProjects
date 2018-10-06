(** I Permutations *)
(* Ex 1 *)
let rec insert x l = 
    match l with
    | [] -> [[x]]
    | head::tail -> (x::l)::
        (List.map (fun lst -> head::lst) (insert x tail))
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

(* 没有使用尾递归，可以保留某些项的顺序 *)
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
  | x::[] -> [[x]] (* 必须写这个特殊情况 *)
  | x::xs -> List.fold_left (fun acc p -> acc @ ins_all_positions x p ) [] (permutation xs)
*)

(** II Arbres binaires *)
type 'a t = V | N of 'a t * 'a * 'a t ;;
type p = Pre | Inf | Post

(* Ex3 *)
let rec print p a = 
    match a,p with
    | V,_ -> ()
    | N(g,x,d), Pre -> 
      Printf.printf "%d" x; print p g; print p d
    | N(g,x,d), Inf ->
      print p g; Printf.printf "%d" x; print p d
    | N(g,x,d), Post ->
      print p g; print p d; Printf.printf "%d" x
;;

(* Ex4 *)
let rec min_max a = 
    match a with
    | V -> assert false
    | N(V,x,V) -> (x,x)
    | N(V,x,a)
    | N(a,x,V) -> let t,z = min_max a in
                min x t,max x z
    | N(g,x,d) -> let tg,zg = min_max g in
                let td,zd = min_max d in
                min x (min tg td), max x (max zg zd)
;;

(* Ex 5 *)