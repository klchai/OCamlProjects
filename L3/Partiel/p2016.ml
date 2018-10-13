(* Q1 *)
let rec f x y =if x < y then print_int x;  y + 2;;

(* Q2 *)

(* Q3 *)
let rec h1 l = 
  match l with
    | [] -> 0
    | [x] -> h1 []
    | x::y::s -> x + h1 s

let rec h2 (x,y) = h2 (y,x - 1)

let rec h3 x = h3 (h3 (x - 1))

(* Q4 *)
let f1 x y z = (x (y<z)) :: z
let rec f2 (x, y) =if x = 0 then y else f2 (y, x)
(* let f3 x = x x *)
let rec f4 x y = f4 y ([]::x)

(* Q5 *)

(* Q6 *)
let bar_r z l = 
  let rec aux l acc = 
    match l with
      | [] -> acc
      | x::s -> aux s acc+x*z
  in
aux l 10

type t = {
  digits : int list;
  base : int;
}

(* Q7 *)
let decompose a b = 
  let rec aux a b = 
    if b<>0 then (b mod a) :: aux a (b/a)
    else []
  in
  {digits = aux a b; base = a;}

(* Q8 *)
let print t = 
  Printf.printf "Nombre : \n";
  List.fold_right (fun a b -> Printf.printf "%d" a)
  t.digits ();
  Printf.printf "\nBase : %d \n" t.base
;;
print (decompose 5 329);;

(* Q9 *)
let to_int t = 
  List.fold_right (fun a b -> b * t.base + a)
  t.digits 0

(* Q10 *)
let add a b = 
  let a_10 = to_int a in
  let b_10 = to_int b in
  decompose a.base (a_10 + b_10)

(* Q11 *)
let positions t = 
  let (x,y) = 
    List.fold_left (fun (list,pos) b -> 
                    if b<> 0 then (pos::list, pos+1)
                    else (list, pos+1))
    ([],0) t.digits
  in
x