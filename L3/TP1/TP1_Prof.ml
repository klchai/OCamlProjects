(* @author Kelun Chai, chaikelun@gmail.com *)

(* 
1) Préférer l'utilisation de définitions comme
> let _ = print_something x
plutôt que 
> print_something x;;

2) Préférer l'utilisation de la syntaxe 
> match t with ... 
plutôt que 
> if t = ... then ... else  ...
lorsque l'on veut inspecter la structure d'un terme t
*)

(** EX 1*)
type interval = { inf : int; sup : int}

let make_interval a b = 
    if a < b 
    then {inf = a; sup = b}
    else {inf = b; sup = a}

let min4 a b c d = 
    min a (min b (min c d))

let max4 a b c d = 
    max a (max b (max c d))

let add_i {inf = i1; sup = s1} {inf = i2; sup = s2} = 
    {inf = min4 (i1+i2) (i1+s1) (i2+s1) (i2+s2);
     sup = max4 (i1+i2) (i1+s1) (i2+s1) (i2+s2)}

let apply_op op {inf = i1; sup = s1}{inf = i2; sup = s2} = 
    {inf = min4 (op i1 i2) (op i1 s1) (op i2 s1) (op i2 s2);
     sup = max4 (op i1 i2) (op i1 s1) (op i2 s1) (op i2 s2)}

let add = apply_op (+)

let sub = apply_op (-)

let mul = apply_op ( * )


(** Ex2 *)
type t = B | N | R

let permute l =
  List.map (fun a -> match a with B -> N | N -> R | R -> B) l

let rec permute_bis l = 
    match l with
    | [] -> []
    | B::l -> N::permute_bis l
    | N::l -> R::permute_bis l
    | R::l -> B::permute_bis l

let compte_B l = 
  List.fold_left (fun a b -> if b=B then a+1 else a) 0 l


let rec compte_B_bis l = 
    match l with
    | [] -> 0
    | B::l -> 1 + compte_B_bis l 
    | _::l -> compte_B_bis l

let is_B = function
    | B -> true
    | _ -> false

let compte_B_bis2 l = 
    List.length (List.filter is_B l)

let plus_grande_sequence_de_B l = 
  let step (actu, best) = function
    | B -> actu+1, max (actu+1) best
    | _ -> 0, best in
  List.fold_left step (0, 0) l |> snd

(* TEST *)
let _ = Printf.printf "=== Test Question 1.3 ===\n"
let _ = Printf.printf "%d\n" (plus_grande_sequence_de_B [B;N;N;B;B;B;R;N;N;B;B;R])

let plus_grande_sequence_de_B_bis l = 
  let rec seq guess count l = 
    match l with
    | [] -> max guess count
    | B::l -> seq guess (count+1) l
    | _::l -> seq (max guess count) 0 l
  in
  seq 0 0 l

let remplace l = 
    let rec last_ele l = 
        match l with
        | [] -> failwith "Empty List"
        | [x] -> x
        | x::rest -> last_ele rest
    in
    List.map (fun a -> match a with | B -> last_ele l; | N -> N; |R -> R;) l

let rec last_val l = 
        match l with
        | [] -> failwith "Empty List"
        | [x] -> x
        | x::rest -> last_val rest

let remplace_bis l = 
    let last = last_val l in
    let rec remp = function
        | B::l -> last::remp l
        | x::l -> last::remp l
        | [] -> [] in
    remp l

(** Ex 3 *)
type monome = {coeff : int; degre : int}
type polynome = monome list

let p = [{coeff = 1; degre = 5};
         {coeff = (-2); degre = 4};
         {coeff = 3; degre = 1};
         {coeff = 1; degre = 0}]

let afficher_m m = 
  let rec translate m = (* seulement besoin de faire un match sur m.degre *)
    match m with
    | {coeff = x; degre = 1} -> Printf.printf "%dX" x
    | {coeff = x; degre = 0} -> Printf.printf "%d" x
    | {coeff = x; degre = y} -> Printf.printf "%dX^%d" x y in
  translate m

let _ = Printf.printf "\n=== Test Question 3.1===\n "
let _ = afficher_m {coeff=3; degre=1}

(* PROBLEME : IMPOSSIBLE D'AFFICHER '+' *)

let afficher_p p = (* attention le type de retour de cette fonction doit être unit *)
    let rec aux l = 
        match l with
        | [] -> []
        | x::rest -> afficher_m x; aux rest in
    aux p

(* Le corrigé de la fonction afficher_p *)
let afficher_p_corrige = function
  | [] -> Printf.printf "0" (* cas du polynôme nul *)
  | m::l -> afficher_m m; (* on met à part le premier coefficient comme le suggère l'énoncé *)
            let rec aff = function
              | [] -> ()
              | m::l ->
                 if m.coeff >= 0 then Printf.printf "+"; (* cette ligne permet d'afficher + *)
                 afficher_m m; aff l in
            aff l

let _ = Printf.printf "\n=== Test Question 3.2===\n "
let _ = afficher_p p

let deriver p = (* cette fonction ne doit pas afficher (pas de printf) et doit renvoyer un polynôme *)
    let rec trans_m m = 
        match m with
        | {coeff = x; degre = 1} -> Printf.printf "%d" x
        | {coeff = x; degre = y} -> Printf.printf "%dX%d" (y-1) ((y-1)*x) in
    let rec aux l = 
        match l with
        | [] -> []
        | x::rest -> trans_m x; aux rest in
    aux p
    
(* le résultat de la dérivation d'un monôme {coeff=x; degre=y} est :
   {coeff=x*y; degre=y-1}
   faire aussi attention à l'invariant, ici le danger vient du fait que x*y = 0 lorsque y = 0 *)
    
let _ = Printf.printf "\n=== Test Question 3.3===\n "
let _ = deriver p