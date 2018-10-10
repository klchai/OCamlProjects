open Sort

module E1 = 
  struct
    type t = int
    let compare x y = x - y
  end 

module T1 = T(E1)

let l1 = [4;1;5;3;2]
let l2 = T1.tri l1
let l3 = T1.insertion 6 l2

let () = 
  Printf.printf "L1: ";
  List.iter (Printf.printf "%d") l1;
  