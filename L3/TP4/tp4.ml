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



let () =
  affiche a0;
  ignore(Graphics.read_key())
           