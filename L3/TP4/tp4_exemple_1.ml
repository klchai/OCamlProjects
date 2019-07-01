open Graphics
open Image

(* 

Placer ici votre code des exercices 1 et 2

*)


(* Renvoie une image sous la forme d'une matrice à partir d'une représentation d'image intermédiare [rle] *)
let mk_matrix rle  =
  let mat = Array.make_matrix 16 16 white in
  List.iteri (fun j l ->
      let color = ref black in
      let i = ref 0 in
      List.iter (fun num ->
          for k = 0 to num - 1 do
            mat.(!i + k).(j) <- !color;
          done;
          i := !i + num;
          if !color == black then color := white
          else color := black;
        ) l;
    ) rle;
  mat

(* Première image en représentation intermédiaire *)
let box =
List.rev [
  [0; 2; 12; 2];
  [0; 1; 1; 12; 1; 1];
  [1; 3; 7; 2; 1; 1; 1];
  [1; 2; 1; 7; 1; 3; 1];
  [1; 1; 1; 9; 1; 2; 1];
  [1; 1; 1; 2; 4; 3; 1; 2; 1];
  [1; 2; 6; 3; 1; 2; 1];
  [1; 4; 1; 5; 1; 3; 1];
  [1; 4; 1; 3; 2; 4; 1];
  [1; 5; 3; 6; 1];
  [1; 4; 1; 3; 1; 5; 1];
  [1; 4; 1; 3; 1; 5; 1];
  [1; 5; 3; 4; 1; 1; 1];
  [1; 10; 2; 2; 1];
  [0; 1; 1; 12; 1; 1];
  [0; 2; 12; 2]
]

(* Deuxième image en représentation intermédiaire *)
let ghost =
List.rev [
   [ 0; 5; 5; 6];
   [ 0; 3; 2; 5; 2; 4];
   [ 0; 2; 1; 9; 1; 3];
   [ 0; 1; 1; 11; 1; 2];
   [ 0; 1; 1; 1; 1; 1; 1; 8; 1; 1];
   [ 1; 2; 1; 1; 1; 4; 3; 2; 1];
   [ 1; 2; 1; 1; 1; 3; 1; 2; 1; 2; 1];
   [ 1; 11; 1; 1; 1; 1];
   [ 1; 1; 1; 1; 1; 1; 1; 4; 1; 3; 1];
   [ 1; 1; 5; 8; 1];
   [ 1; 1; 5; 8; 1];
   [ 0; 1; 1; 1; 5; 7; 1];
   [ 0; 1; 1; 2; 1; 1; 1; 1; 1; 5; 1; 1];
   [ 0; 2; 1; 10; 1; 2];
   [ 0; 3; 2; 6; 2; 3];
   [ 0; 5; 6; 5]
]

(* Troisième image en représentation intermédiaire *)
let star =
List.rev [
   [0; 7; 2; 7];
   [0; 6; 1; 2; 1; 6];
   [0; 6; 1; 2; 1; 6];
   [0; 5; 1; 4; 1; 5];
   [6; 4; 6];
   [1; 14; 1];
   [0; 1; 1; 4; 1; 2; 1; 4; 1; 1];
   [0; 2; 1; 3; 1; 2; 1; 3; 1; 2];
   [0; 3; 1; 2; 1; 2; 1; 2; 1; 3];
   [0; 3; 1; 8; 1; 3];
   [0; 2; 1; 10; 1; 2];
   [0; 2; 1; 10; 1; 2];
   [0; 1; 1; 5; 2; 5; 1; 1];
   [0; 1; 1; 3; 2; 2; 2; 3; 1; 1];
   [1; 2; 2; 6; 2; 2; 1];
   [3; 10; 3]
]

let () =
  let mat = mk_matrix star (* remplacer par box ou star *) in
  let tree = compress mat in (* il s'agit de *VOTRE* fonction [compress] *) 
  affiche tree
