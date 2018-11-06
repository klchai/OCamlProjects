open Graphics
open Image

type image_tree = color t

(* Exemple du TP. *)
let a0 = N (N (F white,F black,F black,F black),F black,
            F white,F black)

let t0 = [|
    [| white ; black ; white ; white |] ;
    [| black ; black ; white ; white |] ;
    [| black ; black ; black ; black |] ;
    [| black ; black ; black ; black |]
  |]

(* Decompression *)

(* Question 1 : get_pixel : int -> int -> int -> image_tree -> color *)
(* Entrees :
 * - x et y : les coordonnées du point (entiers)
 * - longueur : longueur de l'image (int)
 * - arbre : représentation de l'image (image_tree)
 *
 * Sortie : couleur (Graphics.color)
 *
 * Algo :
 * Parcourir l'arbre :
 * - si F c, renvoyer c
 * - si N(so, se, no, ne) :
 *   - si x < len/2 et y < len/2
 *     rappel get_pixel x y len/2 so
 *   - si x >= len/2 et y < len/2
 *     new_x = x - len/2
 *     rappel get_pixel new_x y len/2 se *)

let rec get_pixel x y longueur arbre =
  match arbre with
  | F c -> c
  | N(so, se, no, ne) ->
    let lg_div_2 = longueur/2 in
    if x < lg_div_2 && y < lg_div_2 then
      get_pixel x y lg_div_2 so
    else if x >= lg_div_2 && y < lg_div_2 then
      let new_x = x - lg_div_2 in
      get_pixel new_x y lg_div_2 se
    else if x < lg_div_2 && y >= lg_div_2 then
      let new_y = x - lg_div_2 in
      get_pixel x new_y lg_div_2 no
    else
      let new_x = x - lg_div_2 in
      let new_y = y - lg_div_2 in
      get_pixel new_x new_y lg_div_2 ne

(* Question 2 : image_matrix_of_tree : int -> image_tree -> color array array *)
(* Entrees :
 * - longueur : longueur de l'image (entier)
 * - arbre : représentation par un arbre de l'image (image_tree)
 *
 * Sortie : représentation par un tableau de l'image (color array array)
 *
 * Algo :
 * Créer un tableau et pour chaque élément x (i.e une ligne du tableau
 * qui représente une colonne de l'image)
 *   Créer un tableau et pour chaque élément y (i.e le pixel de la ligne y
 * de la colonne x)
 *     Stocker la couleur du pixel en (x, y) *)

let image_matrix_of_tree longueur arbre =
  (* fonction appliquee pour chaque colonne x *)
  let image_matrix_of_col x =
    (* on cree le tableau de la colonne *)
    Array.init
      longueur
      (* pour chaque element y qui represente une ligne de la colonne x *)
      (fun y ->
         (* on recupere la couleur du pixel en (x, y) *)
         get_pixel x y longueur arbre)
  in
  (* on cree le tableau general *)
  Array.init
    longueur
    (* pour chaque element x qui represente la colonne x de l'image *)
    (fun x ->
       (* on construit le tableau representant la colonne *)
       image_matrix_of_col x)
;;


(* Compression *)

(* Question 1 :
* monochrome_color : color array array -> int -> int -> int -> color -> bool *)
(* Entrees :
 * - matrix : représentation matricielle de l'image (color array array)
 * - x et y : coordonnées (int)
 * - length : taille du carré à considérer (int)
 * - color : couleur à chercher (Graphics.color)
 *
 * Sortie :
 * est-ce le carre de (x,y) a (x+length-1,y+length-1) ne contient que color ? (booléen)
 *
 * Algo :
 * Pour chaque i de x a x+longueur (exclus) \\colonne de l'image, ligne du tableau
 *   Pour chaque j de y a y+longueur (exclus) \\pixel de l'image, element du tableau
 *     Si le pixel n'est pas de la bonne couleur
 *       Soulever une exception
 * Si pas d'erreur, renvoie vrai
 * Si erreur, renvoie faux *)

exception Wrong_color
let monochrome_color matrix x y length color =
  try
    (* on parcourt toutes les colonnes du carre *)
    for i=x to x+length-1 do
      (* on parcourt tous les pixels de la colonne *)
      for j=y to y+length-1 do
        (* si ce n'est pas la bonne couleur *)
        if matrix.(i).(j) != color then
          (* on souleve une exception *)
          raise Wrong_color
      done
    done ;
    (* si on arrive la, c'est qu'il n'y a pas eu d'erreur pendant
     * la boucle : tous les pixels etaient de la bonne couleur *)
    true
  (* si on a eu Wrong_color, c'est qu'un pixel n'etait pas de la bonne couleur *)
  with Wrong_color -> false

(* Question 2 :
* image_tree_of_matrix : color array array -> int -> int -> int -> image_tree *)
(* Entree :
 * - matrix : représentation matricielle de l'image
 * (color array array)
 * - x et y : coordonnées (int)
 * - length : taille du carré à considérer (int)
 *
 * Sortie :
 * représentation en arbre du carré (x,y) -> (x+length-1, y+length-1) (image_tree)
 *
 * Algo :
 * Si le carré est uniformément noir
 *   renvoie F black
 * Si le carré est uniformément blanc
 *   renvoie F white
 * Sinon
 *   calcule récursivement les représentations des 4
 * sous-parties (so, se, no, ne)
 *   renvoie N(so, se, no, ne) *)
let rec image_tree_of_matrix matrix x y length =
  if monochrome_color matrix x y length black then
    F black
  else if monochrome_color matrix x y length white then
    F white
  else
    let len_div_2 = length/2 in
    let x' = x + len_div_2 in
    let y' = y + len_div_2 in
    let so = image_tree_of_matrix matrix x y len_div_2 in
    let se = image_tree_of_matrix matrix x' y len_div_2 in
    let no = image_tree_of_matrix matrix x y' len_div_2 in
    let ne = image_tree_of_matrix matrix x' y' len_div_2 in
    N(so, se, no, ne)

(* Question 3 : compress : color array array -> image_tree *)
(* Entree : matrix : représentation matricielle de l'arbre (color array array)
 *
 * Sortie : représentation en arbre de l'image (image_tree)
 *
 * Algo :
 * appeler image_tree_of_matrix avec x = 0, y = 0 et longueur = taille de l'image *)
let compress matrix =
  image_tree_of_matrix matrix 0 0 (Array.length matrix)

let () =
  print_int (get_pixel 2 1 4 a0);
  let t1 = image_matrix_of_tree 4 a0 in
  (* on affiche le booleen renvoye par la comparaison de t1 et t0 *)
  Printf.printf "\n%b\n" (t1 = t0);
  let b = monochrome_color t0 2 2 2 black in
  Printf.printf "%b\n" b;
  let a1 = image_tree_of_matrix t0 0 0 4 in
  Printf.printf "%b\n" (a1 = a0);
  let a2 = compress t0 in
  Printf.printf "%b\n" (a2 = a0);
  (* affiche a0;
   * ignore(Graphics.read_key()) *)
