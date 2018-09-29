(** Arithmetique d'intervalles **)

(* Question 1 : creer un type intervalle *)
(* on a deux informations, les bornes sup et inf, qui sont des entier :
 * on fait un enregistrement pour pouvoir facilement acceder aux deux infos *)
type interval = { inf : int; sup : int }

(* Question 0 : print_interval : out_channel -> interval -> unit affiche un intervalle *)
(* on affiche avec fprintf. out indique le canal de sortie. i.inf accede au champ inf
 * de l'intervalle i *)
let print_interval out i =
  Printf.fprintf out "{ %i; %i }" i.inf i.sup

(* Question 2: make_interval : int -> int -> interval cree un intervalle
 * a partir de 2 entiers *)
(* Il faut trouver le plus petit pour en faire la borne inferieure. On utilise la fonction
 * min de Pervasives *)
let make_interval i j =
  { inf = min i j;
    sup = max i j }

(* Question 3: additions / soustraction / multiplication *)

(* On commence par creer 2 fonctions intermediaires min4 et max4 qui nous
 * serviront par la suite pour trouver le minimum entre 4 valeurs *)
let min4 a b c d = min (min a b) (min c d)
let max4 a b c d = max (max a b) (max c d)

(* Comme la formule est toujours la meme, on peut ecrire une fonction plus
 * generale qui prendra en parametre la fonction a appliquer, calculera les 4
 * possibilites et creera l'intervalle qui va bien *)
(* apply : (int -> int -> int) -> interval -> interval -> interval *)
let apply op i1 i2 =
  (* on utilise op pour calculer les 4 possibilites *)
  let a = op i1.inf i2.inf in
  let b = op i1.inf i2.sup in
  let c = op i1.sup i2.inf in
  let d = op i1.sup i2.sup in
  (* on utilise min4 et max4 pour calculer les bornes inf et sup *)
  make_interval (min4 a b c d) (max4 a b c d)

(* On peut maintenant creer add sub et mul en utilisant op et en lui passant
 * la fonction qui va bien *)
(* add : interval -> interval -> interval *)
let add i1 i2 = apply ( + ) i1 i2
(* sub : interval -> interval -> interval *)
let sub i1 i2 = apply ( - ) i1 i2
(* mul : interval -> interval -> interval *)
let mul i1 i2 = apply ( * ) i1 i2

(* Pour tester nos fonctions on les appelle dans un let () = foo,
 * où foo sera execute lors de l'execution du programme *)
let () =
  let i1 = make_interval (-2) 3 in
  let i2 = make_interval 5 8 in
  Printf.printf "%a + %a = %a\n"
    print_interval i1
    print_interval i2
    print_interval (add i1 i2);
  Printf.printf "%a - %a = %a\n"
    print_interval i1
    print_interval i2
    print_interval (sub i1 i2);
  Printf.printf "%a * %a = %a\n"
    print_interval i1
    print_interval i2
    print_interval (mul i1 i2)



(** Echauffement sur les listes **)

type t = B | N | R

(* Question 0 : afficher une liste d'elements de type t *)

(* print_t : out_channel -> t -> () affiche un element de type t *)
let print_t out t = Printf.fprintf out "%s"
    (match t with
     | B -> "B"
     | N -> "N"
     | R -> "R")

(* print_list : (out_channel -> 'a -> ()) -> out_channel -> 'a list -> ()
 * affiche une liste, en prenant en parametre la fonction permettant
 * d'afficher un element de la liste *)
let print_list pp out l =
  let rec aux out = function
    | [] -> Printf.fprintf out "]"
    | [x] -> Printf.fprintf out " %a ]" pp x
    | x :: l -> Printf.fprintf out " %a;%a" pp x aux l
  in
  Printf.fprintf out "[%a" aux l

(* Question 1 : permute : t list -> t list renvoie la liste dans laquelle
 * les B ont ete remplaces par des N, N par R, R par B *)

let rec permute l =
  match l with
  (* si la liste est vide, rien a faire *)
  | [] -> []
  | x :: l ->
    (* on utilise un match pour modifier le premier element de la liste,
     * et on recolle au resultat de permute sur la suite de la liste *)
    (match x with
     | B -> N
     | N -> R
     | R -> B) :: permute l

(* variante plus courte *)
(* voir la doc de map du module List *)
let permute = List.map (function B -> N | N -> R | R -> B)

(* Question 2 : compteB : t list -> int renvoie le nombre de B dans la liste *)

(* Premiere option *)
let rec compteB l =
  match l with
  (* liste vide : 0 B *)
  | [] -> 0
  | x :: tl ->
    match x with
    (* si le premier element est un B, on ajoute 1 au nombre de B dans tl *)
    | B -> 1 + compteB tl
    (* sinon on renvoie directement compteB tl *)
    | _ -> compteB tl

(* variante tail-rec *)
let compteB l =
  (* on va vouloir accumuler dans un parametre de la fonction le nombre de B
   * rencontres jusque la, donc on a besoin d'une fonction auxiliaire *)
  let rec aux acc l = match l with
    (* acc est le nombre de B rencontres jusque la *)
    | [] -> acc
    (* on incremente ou non l'accumulateur en fonction de si le premier element est B *)
    | x :: l ->
      aux (match x with B -> acc + 1 | _ -> acc) l
  in
  aux 0 l

(* variante plus courte *)
(* voir la doc de fold_left du module List *)
let compteB = List.fold_left (fun acc x -> match x with B -> acc + 1 | _ -> acc) 0

(* Question 3 : plus_grande_sequence_de_B : t list -> int renvoie la taille de la plus
* grande suite de B presente dans la liste *)
(* NB : f = function | m1 -> e1 | m2 -> e2 equivaut a f x = match x with | m1 -> e1 | m2 -> e2 *)
let plus_grande_sequence_de_B l =
  (* on a besoin de 2 parametres en plus de la liste :
   * - mx, la taille de la plus grande suite rencontree jusque la
   * - cp, la taille de la suite actuelle *)
  let rec aux mx cp = function
    (* on renvoie le max puisque peut-etre que l'actuelle est la plus grande *)
    | [] -> max mx cp
    | x :: l ->
      match x with
      (* on est dans une sequence de B, on incremente cp *)
      | B -> aux mx (cp+1) l
      (* on est plus dans une sequence, on met eventuellement mx a jour *)
      | _ -> aux (max mx cp) 0 l
  in
  aux 0 0 l

(* variante *)
(* f x |> g equivaut a g (f x) *)
let plus_grande_sequence_de_B l =
  List.fold_left
    (fun (mx,cp) x ->
       match x with
       | B -> mx, cp+1
       | _ -> max mx cp, 0)
    (0,0) l
  |> (fun (x,y) -> max x y)

(* Question 4 : remplace : t list -> t list renvoie la liste dans laquelle les B
 * ont ete remplaces par le dernier element *)

(* variante en une passe, non tail-rec *)
(* on va commencer par parcourir la liste a la recherche du dernier element,
 * et au retour on va modifier les B *)
let remplace l =
  let rec aux = function
    (* on a la liste vide, rien a remplacer. On renvoie B "par defaut" *)
    | [] -> B, []
    (* on a le dernier element. On renvoie 2 infos : cet elements, et la liste *)
    | [x] -> x, [x]
    | x :: l ->
      (* on appelle recursivement pour avoir le dernier element et le reste
       * de la liste modifiee *)
      (* on casse le tuple : y recupere le dernier element, l la liste *)
      let y,l = aux l in
      (* si l'element en tete est B, on le remplace par le dernier et on recolle *)
      (* le dernier element est toujours le meme *)
      match x with
      | B -> y, y :: l
      | _ -> y, x :: l
  in
  (* snd : 'a * 'b -> 'b renvoie le deuxieme element d'un couple, ici la liste *)
  snd (aux l)

(* variante en deux passes *)

let remplace l =
  let last = List.fold_left (fun _ x -> x) B l in
  List.map (function B -> last | x -> x) l


let () =
  let lst = [B;N;N;B;B;B;R;N;N;B;B;R] in
  Printf.printf "permute %a = %a\n"
    (print_list print_t) lst
    (print_list print_t) (permute lst);
  Printf.printf "compteB %a = %i\n"
    (print_list print_t) lst
    (compteB lst);
  Printf.printf "plus_grande_sequence_de_B %a = %i\n"
    (print_list print_t) lst
    (plus_grande_sequence_de_B lst);
  Printf.printf "remplace %a = %a\n"
    (print_list print_t) lst
    (print_list print_t) (remplace lst)


(** Polynômes **)

type monome = { coeff : int; degre : int }
type polynome = monome list

let p = [
  {coeff=1;degre=5};
  {coeff=(-2);degre=4};
  {coeff=3;degre=1};
  {coeff=1;degre=0};
]

(* Question 1 : afficher_m : monome -> unit affiche un monome *)
let afficher_m { coeff; degre } =
  if degre = 0 then Printf.printf "%i" coeff
  else if degre = 1 then Printf.printf "%iX" coeff
  else Printf.printf "%iX^%i" coeff degre

(* Question 2 : afficher_p : polynome -> unit affiche un polynome *)
let afficher_p l =
  let rec aux = function
    | [] -> ()
    | m :: l ->
      if m.coeff < 0 then afficher_m m
      else (print_char '+'; afficher_m m);
      aux l
  in
  match l with
  | [] -> ()
  (* on affiche le premier separement pour ne pas mettre de '+' inutile *)
  | m :: l -> afficher_m m; aux l

(* Question 3 : deriver : polynome -> polynome renvoie la derivee d'un polynome *)

(* on derive un monome : (k*x^n)' = k*n*x^(n-1) *)
let deriver_m m = { coeff = m.coeff * m.degre; degre = m.degre - 1 }

let rec deriver p =
  match p with
  | [] -> []
  (* c'est le dernier : s'il est de degre 0 sa derivee est nulle *)
  | [m] -> if m.degre = 0 then [] else [deriver_m m]
  | m :: l -> deriver_m m :: deriver l

(* Question 4 : somme : polynome -> polynome -> polynome renvoie la somme des
 * deux polynomes *)
let rec somme p1 p2 =
  match p1, p2 with
  (* si l'un est vide, on renvoie l'autre *)
  | [], _ -> p2
  | _, [] -> p1
  | m1::l1, m2::l2 ->
    (* si un monome est de degre strictement superieur, on l'ajoute en tete
     * de la liste et recolle au resultat de la somme du reste de sa liste avec
     * l'autre liste *)
    if m1.degre > m2.degre then m1 :: (somme l1 p2)
    else if m1.degre < m2.degre then m2 :: (somme p1 l2)
    (* sinon les 2 monomes sont du meme degre : on les additionne et on recolle
     * avec la somme des deux restes de liste *)
    else
      let m = { coeff = m1.coeff + m2.coeff; degre = m1.degre } in
      (* attention si les deux monomes s'annulent on n'ajoute pas le resultat *)
      if m.coeff = 0 then somme l1 l2
      else m :: (somme l1 l2)

(* Question 5 : evaluer : polynome -> int -> int renvoie la valeur du polynome
 * quand X vaut une valeur donnee *)

(* calcul de puissance : cf l'algorithme du cours 2 d'IPF *)
let rec pow x n =
  if n < 0 then 0 (* ¯\_(ツ)_/¯ *)
  else if n = 0 then 1
  else
    let y = pow x (n/2) in
    if n mod 2 = 0 then y * y else y * y * x

let rec evaluer p x =
  let rec aux acc = function
    | [] -> acc
    (* on evalue le monome en tete et on ajoute le resultat a l'accumulateur *)
    | m :: p -> aux (acc + m.coeff * pow x m.degre) p
  in
  aux 0 p

(* variante plus courte *)

let rec evaluer p x =
  List.fold_left (fun acc m -> acc + m.coeff * pow x m.degre) 0 p


let () =
  afficher_m {coeff=3; degre=1}; print_newline ();
  afficher_m {coeff=4; degre=0}; print_newline ();
  afficher_m {coeff=(-5); degre=2}; print_newline ();
  afficher_p p; print_newline ();
  afficher_p (deriver p); print_newline ();
  afficher_p (somme p (deriver p)); print_newline ();
  print_int (evaluer p 3); print_newline ()
