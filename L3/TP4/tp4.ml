open Graphics
open Image

type image_tree = color t

(* Exemple du TP. *)
let a0 = N (N (F white,F black,F black,F black),F black,F white,F black)

let t0 = [|
    [| white ; black ; white ; white |] ;
    [| black ; black ; white ; white |] ;
    [| black ; black ; black ; black |] ;
    [| black ; black ; black ; black |]
  |]

(* 
let () =
  affiche a0;
  ignore(Graphics.read_key()) 
*)
           
let rec get_pixel x y longueur arbre = 
  match arbre with
    | F(c) -> c
    | N(so,se,no,ne) -> 
        let h = longueur/2 in
          if x<h && y<h then get_pixel x y h so
          else if x>=h && y<h then get_pixel (x-h) y h se
          else if x<h && y>=h then get_pixel x (y-h) h no
          else get_pixel (x-h) (y-h) h ne

let () = Printf.printf "%x\n" (get_pixel 2 1 4 a0)
let () = Printf.printf "%x\n" (get_pixel 0 0 4 a0)

let image_matrix_of_tree l t = 
  Array.init l (fun x->
    Array.init l (fun y ->
      get_pixel x y l t
      )
    )

exception Diff
let monochrome_color image_matrix x y longueur couleur = 
  try
    for x = x to x+longueur-1 do
       for y = y to y+longueur-1 do
         if image_matrix.(x).(y)<>couleur then
           raise Diff
         done
      done;
    true
  with Diff -> false
;;
monochrome_color t0 2 2 2 black;;

let rec image_tree_of_matrix image_matrix x y longueur =
  if monochrome_color image_matrix x y longueur image_matrix.(x).(y) then
    F(image_matrix.(x).(y))
  else let h = longueur / 2 in 
    let so = image_tree_of_matrix image_matrix x y h in
    let se = image_tree_of_matrix image_matrix (x+h) y h in
    let no = image_tree_of_matrix image_matrix x (y+h) h in
    let ne = image_tree_of_matrix image_matrix (x+h) (y+h) h in
  N(so,se,no,ne)
;;
image_tree_of_matrix t0 0 0 4;;

let compress image_matrix = 
  image_tree_of_matrix image_matrix 0 0 (Array.length image_matrix);;
