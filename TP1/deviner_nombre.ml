open Random;;
Random.self_init ();;

(* 初始化随机数 *)
let nb = Random.int 1000;;
(* 获取随机数 *)
(* Printf.printf "%d\n" nb;; *)
(* 尝试次数 *)
let time = 10;;
Printf.printf "====== DEVINER NOMBRE ======\nVous avez %d fois pour essayer.\n" time;;

let rec devine t_try = 
  (* 如果超过尝试次数，失败 *)
  if t_try >= time then Printf.printf "Echec ! \n"
  else
    (* 用户输入 *)
    let n' = read_int () in
     if n' = nb then
        Printf.printf "Bravo ! Vous avez atteint le resultat en %d tentatives.\n" t_try
     else
       begin
         (* 如果输入过小，提示+ *)
         if n' < nb then
           Printf.printf "+ "
         (* 如果输入过大，提示- *)
         else if n' > nb then
           Printf.printf "- ";
     Printf.printf"(il vous reste %d tentatives)\n" (time-t_try);
   devine (t_try+1)
 end
;;

devine 1;;
