type t = F of int array | N of int array * t array

let rec print = function
  | F a -> Array.iter (fun v -> Printf.printf "%d " v) a
  | N (a, t) -> 
    for i = 0 to Array.length a - 1 do
      print t.(i); Printf.printf "%d" a.(i)
    done;
    print t.(Array.length t - 1)

exception Break of int 
exception Found

let lookup v t = try
  let l = Array.length t in
  for i = 0 to l-1 do 
    if v < t.(i) then raise Not_found (*Break(i)*)
    else if v = t.(i) then raise Found
  done; l
  with Break(n)->n

let rec search v t = 
  try
    match t with
      | F a -> lookup v a
      | N (a,t) ->
        let n = lookup v a in
        search v t.(n)
  with Not_found -> 0 (* false *)
    | Found -> 1 (* true *)

(* let insert_array i v t = 
  Array.init(fun i' -> if i'<i then t.(i)
                       else if i'=i then v
                       else t.(i+1))
                       (Array.length t + 1) *)

(* let eclatement t = 
  let m = (Array.length t) / 2 in
  let tb = Array.init(fun i -> t.(i)) m in
  let ta = Array.init (fun i -> t.(i+m+1)) ((Array.length t)-m) in
  t.(m),tb,ta *)