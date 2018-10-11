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