(** 1. ÉCHAUFFEMENT : ITÉRATEURS **)


let affiche_liste_poly aff l = (* fonction polymorphe d'affichage d'une liste à partir d'une fonction d'affichage d'un élément aff *)
  let stdaff = fun out -> aff in
  Printf.printf "[";
  begin match l with
  | [] -> ()
  | h::t -> Printf.printf "%a" stdaff h;
            List.iter (Printf.printf "; %a" stdaff) t
  end;
  Printf.printf "]"


let affiche_liste_entiers =
  affiche_liste_poly (Printf.printf "%d")
  
let count a l =
  List.fold_left (fun acc h -> acc + if h = a then 1 else 0) 0 l
  
let flatten l =
  List.fold_right (@) l [] 

let fst_list l =
  List.map fst l

  
(** 2. LE TRI FUSION **)

  
(* 2.4 CRÉATION D'UNE LISTE QUELCONQUE *)
  
let _ = Random.self_init ()
      
let rec rdm_int_list bound len acc = (* ajout d'une liste de taille len de nombres aléatoires entre 0 et bound (exclu) à la liste acc *)
  if len = 0
  then acc 
  else let ne = Random.int bound in
       rdm_int_list bound (len-1) (ne::acc)

let make_list n = rdm_int_list (n-1) n []

(* 2.1 COUPER *)
           
let rec couper1 l = match l with
  | [] | [_] -> l, []
  | h1::h2::t -> let l1, l2 = couper1 t in
                 h1::l1, h2::l2

let l5 = make_list 5
let _ = couper1 l5
let l6 = make_list 5
let _ = couper1 l6
      
let couper2 l =
  let rec couperrec ((l1, l2) as acc) = function
  | [] -> acc
  | h::t -> couperrec (l2, h::l1) t in
  couperrec ([], [])  l

let l7 = make_list 7
let _ = couper2 l7
let l8 = make_list 8
let _ = couper2 l8

let couper3 l =
  List.fold_left (fun (l1, l2) h -> l2, h::l1) ([], []) l

let l9 = make_list 9
let _ = couper3 l9
let l10 = make_list 10
let _ = couper3 l10

(* 2.2 FUSION *)
      
let rec fusion comp l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | h1::t1, h2::t2 ->
     if comp h1 h2 <= 0
     then h1 :: fusion comp t1 l2
     else h2 :: fusion comp l1 t2 

let _ = fusion compare [1; 3; 5; 7] [0; 2; 4; 5; 8]
    
let fusion_rt comp l1 l2 =   (* version récursive terminale avec accumulateur *)
  let rec fusionrec l1 l2 acc =
    match l1, l2 with
    | [], l | l, [] ->
       List.rev_append acc l (* attention à n'utiliser ni List.rev ni List.append ici *)
    | h1::t1, h2::t2 ->
       if comp h1 h2 <= 0
       then fusionrec t1 l2 (h1 :: acc)
       else fusionrec l1 t2 (h2 :: acc) in
  fusionrec l1 l2 []

let _ = fusion_rt compare [1; 3; 5; 7] [0; 2; 4; 5; 8]
  
(* 2.3 TRI FUSION *)  
  
let rec trierc couper comp l =
  match l with 
  | [] | [_] -> l
  | _ -> let l1, l2 = couper l in
         let tl1 = trierc couper comp l1 in
         let tl2 = trierc couper comp l2 in
         fusion comp tl1 tl2

let trier comp l = trierc couper1 comp l
         
let rec trier_cps comp l cont = (* version récursive terminale avec continuation *)
  match l with
  | [] | [_] -> cont l
  | _ -> let l1, l2 = couper3 l in (* couper3 est récursive terminale *)
         trier_cps comp l1 (fun tl1 ->
         trier_cps comp l2 (fun tl2 ->
         cont (fusion_rt comp tl1 tl2)))

let trier_rt comp l = trier_cps comp l (fun x -> x)

let l12 = make_list 12
let _ = trier compare l12
let _ = trier_rt compare l12
let l13 = make_list 13
let _ = trier compare l13
let _ = trier_rt compare l13
let l14 = make_list 14
let _ = trier compare l14
let _ = trier_rt compare l14

(*let _ = trier compare (make_list 1000000)          (* stack overflow sur ma machine *) *)
let _ = trier_rt compare (make_list 1000000)       (* OK *)


(** 3. TEST DE STABILITÉ **)

          
(* 3.1 NUMÉROTATION *)  

let numerotation l =
  List.fold_left (fun (accl, accn) h -> (* l'accumulateur contient aussi le numéro à donner au prochain élément *)
      (h, accn)::accl, accn + 1) ([], 1) l
  |> fst
  |> List.rev (* on a utilisé fold_left, il faut donc remettre les éléments dans le bon ordre *)

let _ = numerotation ['a'; 'b'; 'c'; 'c']

(* 3.2 FONCTION DE COMPARAISON *)

let comp_assoc (a1, a2) (b1, b2) =
  compare a1 b1

let _ = comp_assoc (1, 4) (2, 1)

(* 3.3 AFFICHAGE *)          

let affiche_liste = affiche_liste_poly (fun (a,b) -> Printf.printf "(%d, %d)" a b)
          
let _ = affiche_liste [(1,3); (4, 9); (0, 8)]

(* 3.4 STABILITÉ DU TRI FUSION *)

let numerote_trie_affiche tri l = 
  numerotation l
  |> tri comp_assoc
  |> affiche_liste

let l = [30; 20; 10; 30] 
let _ = numerote_trie_affiche trier l (* OK *)
      
let l' = [20; 30; 30; 10]       
let _ = numerote_trie_affiche trier l' (* PAS OK *)
(*  => PAS STABLE *)

let couper4 l =
  let n = List.length l in
  let rec take k l = 
    if k = 0
    then [], l
    else match l with
         | [] -> assert false
         | h::t -> let l1, l2 = take (k-1) t in
                   h::l1, l2 in
  take (n/2) l
         
let _ = numerote_trie_affiche (trierc couper4) l (* OK *)

let _ = numerote_trie_affiche (trierc couper4) l' (* OK *)
(* => STABLE ? *)
