(* 参考年 *)
let annee_ref = 2016;;
(* 测试是否闰年 *)
let est_bissextile annee = 
  (annee mod 4 = 0 && annee mod 100 <> 0) || (annee mod 400 = 0)
;;

(* 一年中的天数 *)
let rec jours_entre_annees a1 a2 acc = 
  (*如果两个年份一样，则年之间的日数差为acc*)
  if a1 = a2 then acc
  else
   (*闰年366天，否则365天*)
   let nbj = if est_bissextile a2 then 366 else 365 in
   let new_a = a2 -1 in
   jours_entre_annees a1 new_a (acc + nbj)
;;

let jours_a_annee a = jours_entre_annees a annee_ref 0
;;

(* 参考月份：12月 *)
let mois_ref = 12;;

(* Nombre de Jours *)
let nb_jours m b =
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if b then 29 else 28
  | _ -> assert false
;;

(* Jours entre Mois *)
let rec jours_entre_mois m1 m2 b acc = 
  if m1 = m2 then acc
  else
    let nbj = nb_jours m2 b in
    let new_m = m2 - 1 in
  jours_entre_mois m1 new_m b (acc + nbj)
;;

let jours_a_mois m b acc = jours_entre_mois m mois_ref b acc
;;

(** JOURS *)
let jour_corres_ecart = function
  | 6 -> "dimanche"
  | 5 -> "lundi"
  | 4 -> "mardi"
  | 3 -> "mercredi"
  | 2 -> "jeudi"
  | 1 -> "vendredi"
  | 0 -> "samedi"
  | _ -> assert false
;;

let total_jours d m b acc = acc + (nb_jours m b) - d;;

let ecart acc = acc mod 7;;

let date_a_jour j m a = 
  let b = est_bissextile a in
  let acc = jours_a_annee a in
  let acc = jours_a_mois m b acc in
  let acc = total_jours j m b acc in
  let res = ecart acc in
  let day = jour_corres_ecart res in
  Printf.printf "Ce jour etait un %s.\n" day
;;

let rec main() = 
  let j = Printf.printf "jour  : "; read_int() in
  let m = Printf.printf "mois  : "; read_int() in
  let a = Printf.printf "annee : "; read_int() in
date_a_jour j m a;
main()
;;

main();;