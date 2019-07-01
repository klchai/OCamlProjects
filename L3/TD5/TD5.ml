(* Q1 *)
let sum_cube l = 
  let rec aux l acc = 
    match l with
      | [] -> acc
      | x::s -> aux s (x*x*x+acc)
  in
aux l 0

(* Q5 *)
let rec ack m n = 
  match n,m with
    | _,0 -> n+1
    | 0,_ -> ack (m-1) 1
    | n,m -> ack (m-1) (ack m (n-1))

(* Q6 *)
let ack = 
  let h = Hashtbl.create 42 in
  let rec ack m n = 
    try Hashtbl.find h (m,n)
    with Not_found ->
      let res = match n,m with
        | _,0 -> n+1 
        | 0,_ -> ack (m-1) 1
        | n,m -> ack (m-1) (ack m (n-1))
      in
      Hashtbl.add h (m,n) res; res
  in
ack