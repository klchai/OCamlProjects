(** Partiel 2017 *)
(* Ex 1 *)
let f1 (x, y) z = if z then x :: [y] else []

let f2 x y = 4 + ((y x) 1)

let rec f3 x = not (f3 x)

(*
let rec f4 x =
  match x with
    | [] -> []
    | z :: _ -> f4 z
*)

(* Ex 2 *)
let somme n =
  let rec aux n acc = 
    if n = 0 then acc
    else aux (n-1) (acc+n*n)
  in 
aux n 0

let rec foo f x =
  if x = 0 then
    f x
  else
    foo (fun y -> f (x + y)) (x-1)

(* Ex 3 *)
(* Q5 *)
let intersection l1 l2 = 
  (* let rec mem x l = 
    match l with
      | [] -> false
      | head::tail ->head = x || mem x tail
  in *)
  let rec aux l1 l2 = 
    match l1 with
      | [] -> []
      | head::tail -> if List.mem head l2 then head::(aux tail l2) else aux tail l2
  in
aux l1 l2

let inter l1 l2 = List.filter (fun x -> List.mem x l2) l1

let intersection l1 l2 = 
  let rec inters acc l1 l2 = 
    match l1,l2 with
      | hd1::tl1, hd2::tl2 ->
        if hd1=hd2 then inters (hd1::acc) tl1 tl2
        else if hd1<hd2 then inters acc tl1 l2
        else inters acc l1 tl2
      | _,_ -> acc
  in 
List.rev (inters [] l1 l2)
(* Q6 *)
(* let duplique l = 
  let rec aux l acc = 
    match l with
      | [] -> List.rev acc
      | head::tail -> aux tail (head::head::acc)
  in
aux l [] *)
let duplique l = List.fold_right (fun x res -> x::x::res) l []
(* Q7 *)
let nombre_paire_d_elements l = (List.length l) mod 2 = 0
