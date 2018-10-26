type t = { mots : string list ; branches : (int * t) list }

(* Q1 *)
let empty = {mots=[];branches=[]}

(* Q2 *)
let rec find l t = 
  match l with
    | [] -> t.mots
    | n::s -> find s (List.assoc n t.branches)

(* Q3 *)
let rec change_assoc x v l = 
  match l with
    | [] -> [(x,v)]
    | (x',v') ::s -> if x = x' then (x,v)::s
                     else (x',v')::change_assoc x v s 