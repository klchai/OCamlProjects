type point = {x : int; y : int};;

let new_point x y = {x; y};;

type quadrilatere = {p1 : point; p2 : point; p3 : point; p4 : point};;

let new_quadrilatere p1 p2 p3 p4 = {p1; p2; p3; p4};;

let vect p1 p2 = {x = p2.x - p1.x; y = p2.y - p1.y};;

let lg2_vect {x; y} = x * x + y * y;;

let vects {p1; p2; p3; p4} = vect p1 p2, vect p2 p3, vect p3 p4, vect p4 p1;;

let is_rectangle q =
  let p1p2, p2p3, p3p4, p4p1 = vects q in
  p1p2.x * p2p3.x + p1p2.y * p2p3.y = 0
  && p2p3.x * p3p4.x + p2p3.y * p3p4.y = 0
    && p3p4.x * p4p1.x + p3p4.y * p4p1.y = 0
;;

let is_losange q = 
  let p1p2, p2p3, p3p4, p4p1 = vects q in
  let lp1p2 = lg2_vect p1p2 in
  let lp2p3 = lg2_vect p2p3 in
  let lp3p4 = lg2_vect p3p4 in
  let lp4p1 = lg2_vect p4p1 in
  lp1p2 = lp2p3 && lp2p3 = lp3p4 && lp3p4 = lp4p1
;;

let is_carre q = is_losange q && is_rectangle q;;

type figure = 
 | Rectangle of quadrilatere
 | Losange of quadrilatere
 | Carre of quadrilatere
 | Autre of quadrilatere
;;

let to_figure q = 
  let r = is_rectangle q in
  let l = is_losange q in
  match r, l with
   | true, true -> Carre q
   | true, _ -> Rectangle q
   | _, true -> Losange q
   | _, _ -> Autre q
;;

(*为什么要另一个to_figure*)
let to_figure q = 
  match is_rectangle q with
   | true -> begin
    match is_losange q with
     | true -> Carre q
     | false -> Rectangle q
    end
   | false -> begin
     match is_losange q with
     | true -> Losange q
     | false -> Autre q
     end
;;

let compare f1 f2 = 
  match f1, f2 with
  | Carre _, Carre _ -> 0
  | Carre _, _ -> -1
  | _, Carre _ -> 1
  | Rectangle _, Rectangle _ -> 0
  | Rectangle _, _ -> -1
  | _, Rectangle _ -> 1
  | Losange _, Losange _ -> 0
  | Losange _, _ -> -1
  | _, Losange _ -> 1
  | Autre _, Autre _ -> 0
;;
let pp_point fmt p = Format.fprintf fmt "@[(%d,@ %d)@]" p.x p.y;;
let p1 = new_point 0 0;;
let p2 = new_point 0 2;;
let p3 = new_point 2 2;;
let p4 = new_point 2 0;;

let () = Format.printf "p1=%a, p2=%a, p3=%a, p4=%a@." pp_point p1 pp_point p2 pp_point p3 pp_point p4;;

let q1 = new_quadrilatere p1 p2 p3 p4;;
let () = Format.printf "q1:rectangle: %b; losange: %b; carre: %b@." (is_rectangle q1)(is_losange q1)(is_carre q1);;