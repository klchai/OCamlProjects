let ajoute_liste l = 
    let rec aux acc l =
    match l with
     | [] -> acc
     | n::rest -> aux (acc + n) rest
    in
aux 0 l
;;

(** TEST *)
Printf.printf "Test de \"ajoute_liste\"\n%d\n" (ajoute_liste [5; 27; 8; 2]);;

let concatene_liste l = 
    let rec aux acc l = 
    match l with
    | [] -> acc
    | n :: rest -> aux (acc ^ n) rest
    in
aux "" l
;;

(** TEST *)
Printf.printf "Test de \"concatene_liste\"\n%s\n" (concatene_liste ["This"; "is"; "a"; "string"]);;

let concatene_liste_avec_separateur sep l =
    let rec aux acc l =
    match l with
    | [] -> acc
    | n :: [] -> acc ^ n (* [n] -> acc ^ n *)
    | n :: rest -> aux (acc ^ n ^ sep) rest
    in
aux "" l
;;
(** TEST *)
Printf.printf "Test de \"concatene_liste_avec_separateur\"\n";;
Printf.printf "%s\n" (concatene_liste_avec_separateur " " ["This"; "is"; "a"; "string"]);;
Printf.printf "%s\n" (concatene_liste_avec_separateur ", " ["1"; "2"; "3"; "4"]);;
