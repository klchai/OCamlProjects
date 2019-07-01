(* ocamlc -o toto graphics.cma TP6.ml *)
#load "graphics.cma";;
open Graphics;;
type grille = bool array array

let creation n = 
  Array.make_matrix n n false

let init_centre g n = 
  g.(n/2).(n/2) <- true

let () = Random.self_init ()

let init_aleatoire p g = 
  let l = Array.length g in
  for i = 0 to l-1 do
    for j = 0 to l-1 do
      g.(i).(j) <- Random.int 100<p
    done
  done

(* ignore (Graphics.wait_next_event [Graphics.key_pressed]) *)

type etat = { up : bool; down: bool; right : bool; left : bool }
