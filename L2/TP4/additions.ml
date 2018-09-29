(* 
类型： int list -> int 
功能：返回列表中所有元素之和 
思路：做一个遍历
*)
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

(*
类型：string list -> string
功能：串联两个列表
思路：用^运算符直接串联
*)
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

(*
类型：string -> string list -> string
功能：串联列表l中以sep分隔的元素
思路：分类考虑，若为空则返回acc累加器；若只有一个元素，返回acc累加器与该元素；若有多元素，做遍历
*)
let concatene_liste_avec_separateur sep l =
    let rec aux acc l =
    match l with
    | [] -> acc
    | n :: [] -> acc ^ n (* 或者：[n] -> acc ^ n *)
    | n :: rest -> aux (acc ^ n ^ sep) rest
    in
aux "" l
;;
(** TEST *)
Printf.printf "Test de \"concatene_liste_avec_separateur\"\n";;
Printf.printf "%s\n" (concatene_liste_avec_separateur " " ["This"; "is"; "a"; "string"]);;
Printf.printf "%s\n" (concatene_liste_avec_separateur ", " ["1"; "2"; "3"; "4"]);;
