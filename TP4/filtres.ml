(* List.rev *)
let rec rev = function
    | [] -> []
    | n::rest -> (rev rest) @ [n]
;;

let moins_que x l =
    let rec aux acc l =
    match l with
    | [] -> acc
    | n::rest -> aux (if n < x then n::acc else acc) rest
    in
aux [] l
;;

(** TEST *)
let l = [55; 4; 81; 42; 12];;
Printf.printf "Test de \"moins_que\"\n";;
let () = List.iter (Printf.printf "%d ") (moins_que 42 l);;

let taille_chaine_moins_que x l =
    let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | n::rest -> aux (if String.length n < x then n::acc else acc) rest
    in
aux [] l
;;

(** TEST *)
let l1 = ["This";"is";"a";"List"];;
Printf.printf "\nTest de \"taille_chaine_moins_que\"\n";;
let () = List.iter (Printf.printf "%s ") (taille_chaine_moins_que 3 l1);;

let est_carre l =
    let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | n::rest -> let sqrt_of_n = int_of_float (sqrt(float n)) in
                aux (if sqrt_of_n * sqrt_of_n = n then n::acc else acc) rest
    in
aux [] l
;;

(** TEST *)
let l3 = [55;4;81;42;12];;
Printf.printf "\nTest de \"est_carre\"\n";;
let () = List.iter (Printf.printf "%d ") (est_carre l3);;

let filtre_liste f l = 
    let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | n::rest -> aux (if f n then n::acc else acc) rest
    in
aux [] l
;;

let moins_que x = 
    filtre_liste (fun n -> n < x);;
let taille_chaine_moins_que x = 
    filtre_liste (fun n -> String.length n < x);;
let est_carre = 
    filtre_liste (fun n -> let sqrt_of_n = int_of_float (sqrt (float n)) in 
                  sqrt_of_n * sqrt_of_n = n);;