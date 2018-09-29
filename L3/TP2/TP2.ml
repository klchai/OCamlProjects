(* @author Kelun Chai *)
(* TP2 *)

(** Ex1 *)
let affiche_liste_entiers l = List.iter (fun x -> print_int x; print_char ' ') l;;

let count x l = List.fold_left (fun acc y -> if x=y then acc+1 else acc) 0 l;;

let flatten l = List.fold_right (fun a b -> List.append a b) l [];;

(* fst源代码：
   external fst : 'a * 'b -> 'a = "%field0" *)
let fst_list l = List.map fst l;;

let fst_list_right l = List.fold_right (fun a b -> let (x,y) = a in x::b) l [];;

let fst_list_left l = List.fold_left (fun b a-> let (x,y) = a in x::b) [] l;;

(** Ex2 *)
let couper_1 l =
    let rec aux l1 l2 l = 
        match l with
        | [] -> l1,l2
        | [x] -> x::l1,l2
        | x::y::l -> aux (x::l1) (y::l2) l
    in
aux [] [] l
;;

let couper_2 l = 
    let rec aux b l1 l2 l =
        match l with
        | [] -> l1,l2
        | x::s -> if b then aux (not b) (x::l1) l2 s
                  else aux (not b) l1 (x::l2) s
    in
aux true [] [] l
;;

let couper_3 l = 
    List.fold_left
    (fun (b,(l1,l2)) x -> not b, if b then (x::l1,l2) else (l1,x::l2))
    (true,([],[])) l
|> snd
;;

(* comp x y = 0 si x=y
   comp x y < 0 si x<y
   comp x y > 0 si x>y *)
let rec fusion comp l1 l2 = 
    match l1,l2 with
    | _,[] -> l1
    | [],_ -> l2
    | s1::r1,s2::r2 -> if comp s1 s2 < 0 then s1::(fusion comp r1 l2)
                                         else s2::(fusion comp l1 r2)
;;

let rec trier comp l = 
    match l with
    | [] -> []
    | [x] -> [x]
    | _ -> let (l1,l2) = couper_3 l in
           fusion comp (trier comp l1) (trier comp l2)
;;

