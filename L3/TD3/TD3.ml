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

let inter_fct l1 l2 = List.filter (fun x -> List.mem x l2) l1

(* Q2 *)
let duplique l = 
  let rec aux l acc = 
    match l with
      | [] -> List.rev acc
      | head::tail -> aux tail (head::head::acc)
  in
aux l []

(* Q3 *)
let nombre_paire_d_elements l = 
  if (List.length l) mod 2 == 0 
    then true
  else false