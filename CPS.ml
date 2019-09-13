(* Recursive version *)
let rec fib_o n = 
  if n <= 1 then n
  else fib_o (n-1) + fib_o (n-2)

(* Tail-recursive version *)
let fibo n = 
  let rec fib_t n acc1 acc2 = 
    if n = 0 then acc2
    else fib_t (n-1) acc2 (acc1+acc2)
  in
fib_t n 1 0

(* How to explain fun *)
let average x y = (x +. y) /. 2.
let average = fun x y -> (x +. y) /. 2.
let average = fun x -> fun y -> (x +. y) /. 2.

(* CPS Version *)
let fib n = 
  let rec fib_cps n cont = 
    if n <= 1 then cont (n)
    else
      fib_cps (n-1) (fun a -> fib_cps (n-2) 
                     (fun b -> cont (a+b)))
  in
fib_cps n (fun x -> x)

(** Trace for fib cps *)
(*
# fib 3
=> fib_cps 3 (fun x -> x)
=> fib_cps 2
(fun a -> fib_cps 1
 (fun b -> (fun x -> x) (fib_cps 1 + b)))
=> fib_cps 2
(fun a -> (fun x -> x) (1)
 (fun b -> (fun x -> x) ((fun x -> x) (1) + b)))
=> fib_cps 1
(fun a -> fib_cps 0
  (fun b -> (fun x -> x) (fib_cps 0 + ((fun x -> x) (1) + b)))
 )
=> (fun b -> (fun x -> x) (fib_cps 0 + ((fun x -> x) (1) + b))) (fun x -> x) (1)
=> (fun x -> x) ((fun x -> x) (0) + ((fun x -> x) (1) + (fun x -> x) (1)))
=> (fun x -> x) (2)
=> 2
*)

(* Output *)
let _ = 
  let n = 
    try int_of_string Sys.argv.(1)
    with Invalid_argument _ -> 1 in
  Printf.printf "%d\n" (fib n)