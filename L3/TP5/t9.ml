type t = { mots : string list ; branches : (int * t) list }

(* Q1 *)
let empty = {mots = []; branches = []}

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
;;
(* Q4 *)
let rec add l m a = 
  match l with
    (* | [] -> {mots = m::a.mots; branches = a.branches} *)
    | [] -> if List.exists (fun m'->m=m') a.mots then a
            else (* {mots = m::a.mots; branches = a.branches} *)
                 { a with mots = m::a.mots }
    | n::s -> let a' = add s m (try List.assoc n a.branches with Not_found -> empty) in
              {mots = a.mots; branches = change_assoc n a' a.branches}