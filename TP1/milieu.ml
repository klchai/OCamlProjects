(* 输入四个数字 *)
let () =
  Printf.printf "x1 : ";
  let x1 = float_of_int (read_int()) in
  
  Printf.printf "y1 : ";
  let y1 = float_of_int (read_int()) in

  Printf.printf "x2 : ";
  let x2 = float_of_int (read_int()) in

  Printf.printf "y2 : ";
  let y2 = float_of_int (read_int()) in
(* x的均值 *)
(* float(x1+x2)/. 2. *)
(* (float x1 +. float x2) /. 2. *)
  let moy_x = (x1 +. x2) /. 2.0 in
(* y的均值 *)
  let moy_y = (y1 +. y2) /. 2.0 in
(* x y 计算距离 *)
  let len = sqrt ((x1 -. x2) *. (x1 -. x2) +. (y1 -. y2) *. (y1 -. y2)) in
  Printf.printf "(%f, %f)(%f, %f) : (%f, %f), %f\n" x1 y1 x2 y2 moy_x moy_y len
;;
