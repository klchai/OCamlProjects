(* Q1 *)
let intersection l1 l2 = 
  let rec aux l1 l2 = 
    match l1 with
      | [] -> []
      | head::tail -> if List.mem head l2 
                        then head::(aux tail l2) 
                      else aux tail l2
  in
aux l1 l2

let rec intersection_a l1 l2 = 
  match l1,l2 with
    | [],_
    | _,[] -> []
    | h1::t1,h2::t2 -> if h1<h2 then intersection_a t1 l2
                  else if h1>h2 then intersection_a l1 t2
                  else h1::intersection_a t1 t2
        
let inter_fct l1 l2 = List.filter (fun x -> List.mem x l2) l1

(* Q2 *)
let duplique l = 
  List.fold_right (fun x res -> x::x::res) l []

(* Q3 *)
let nombre_paire_d_elements l =  (List.length l) mod 2 == 0

let rec npde l = 
  match l with
    | [] -> true
    | [_] -> false
    | _::_::s -> npde s

(* Q4 *)
exception Empty_list of string
let second_max l = 
  match l with
    | [] -> raise (Empty_list "Liste Vide")
    | head::tail -> snd (List.fold_left (fun (max,sndMax) x -> 
                                         if x<sndMax then (max,sndMax)
                                         else if x>max then (x,max)
                                         else (max,x)) (head,head) tail)

(* Q5 *)
(*let compress l = 
  match l with
    | [] -> []
    | head::tail -> let rec aux (current,count) = function 
                      | [] -> (current,count)
                      | x::s when head=current -> aux (current,count+1) s
                      | x::s -> aux (x,1) s
                    in
                    aux (head,1) tail
*)
let rec compress acc l = 
  match l with
    | [] -> []
    | [x] -> [(x,acc+1)]
    | x::y::s when x=y -> compress (acc+1) (y::s)
    | x::s -> (x,acc+1)::compress 0 s
(* Q6 *)
let rec ajoute_a_liste (v,n) l =
  if n>0 then ajoute_a_liste (v,n-1) (v::l) else l

(* Q7 *)
let decompression l = 
  List.fold_left (fun res (v,n) -> ajoute_a_liste (v,n) res) [] l
let decomp2 l = 
  List.fold_right ajoute_a_liste l []
let decomp3 l =
  List.fold_right (fun (v,n) res -> ajoute_a_liste (v,n) res) []