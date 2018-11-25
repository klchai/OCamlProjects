(** Partiel 2018 *)
(* Q1 *)
let f1 (u,v,w) = if u < w then [v] else []

let rec f2 x y = f2 [y] ()

(* let rec f3 x = not (f3 (f3 (x+1))) *)

let rec f4 f x = 
  match f x with
    | [] -> [""]
    | z::s -> f4 f (x-1)

(* Q2 *)
let f l = 
  let rec aux acc = function
    | [] -> acc
    | (x,y)::s -> aux (x*y+acc) s
  in
aux 0 l

(* Q3 *)
type t = E | N of int * t * t
let add t i = 
  let rec add_cps t i what_remains = 
  match t with
    | E -> what_remains E
    | N(x,g,d) ->
        add_cps g i (fun g' ->
          add_cps d i (fun d' ->
              what_remains (N(x+i,g,d))
            )
        )
  in
add_cps t i (fun x->x)

(* Q4 *)
type block = { index : int; prev_hash : int; merkel_root : int}
type chain = block list
let hash x = 3

let genesis = { index = 0; prev_hash = -1; merkel_root = -1}
(* Q5 *)
let add_block mr bc = 
  let h,tl = match bc with
    | [] -> genesis,[]
    | h::tl -> h,tl
  in
  let b = { index = h.index+1; prev_hash = hash h; merkel_root = mr}
  in
  b::h::tl

(* Q6 *)
exception Integrity_Violation
let check_integrity = function
  | [] -> false
  | [g] -> (g=genesis)
  | h::tl ->
      let f next b = 
        if next.index != b.index+1
        || next.prev_hash != hash b
        then raise Integrity_Violation
        else b
      in
    try (genesis = List.fold_left f h tl)
      with Integrity_Violation -> false

let rec check2 = function
  | [] -> false
  | [g] -> (g = genesis)
  | a::b::tl -> (a.index = b.index+1) && (a.prev_hash = hash b)
                && (check2 (b::tl))

(* Q7 *)
type merkel = E | N of int * merkel * merkel * int

(* Q8 *)
let value = function 
  | E -> raise Not_found
  | N(x,_,_,_) -> x

(* Q9 *)
let make_leaf v = 
  N(hash v, E, E, 0)

(* Q10 *)
let make_node g d = 
  match g,d with
    | N(x,_,_,lx), N(y,_,_,ly) ->
        N(x+y,g,d,(max lx ly)+1)
    | _,_ -> assert false

(* Q11 *)
let rec fusion = function 
  | x::y::s -> make_node x y::fusion s
  | l -> l

(* Q12 *)
let rec merkel_of_list = function
  | [] -> E
  | [x] -> x
  | l -> merkel_of_list (fusion l)
