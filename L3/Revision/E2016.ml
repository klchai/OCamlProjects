(** I.Typage *)
let f1 g (x,y) = (g,x,y)

(* -> 后应该是 int 而不是 unit 类型 *)
(** 为什么？ *)
(* let f2 x y = try x / y with Division_by_zero -> print_string "erreur" *)

(* 最后给 x 赋值，直接给 y 就可以，不需要 ref *)
(** 为毛？ *)
(* let f3 x y = if !x < y then x := ref y *)

(* 1 为 int 实际应该是 float *)
(* let rec f4 (x,y) = (f4 (y, x-1)) +. 1 *)

(** II. Evaluation *)
type t = { mutable a : int list; b : bool }
let rec f x = 
  match x.a with
    | [] -> raise Not_found
    | _::l -> x.a <- l; f x

let rec g x = 
  match x.a with 
    | [] -> raise Not_found
    | _::l -> f {x with a = l}

let v1 = 
  let x = {a=[1;2] ; b=false} in
  try f x with Not_found -> x.a
(* v1:int list = [] *)

let v2 = 
  let x = {a=[1;2] ; b=false} in
  try g x with Not_found -> x.a
(* v2:int list = [1;2] *)

(** III.Systeme fichiers Linux *)
type dir = {
    mutable name : string;
    mutable subdirs : dir list;
    up : dir;
}

let rec root = { name = "/"; subdirs = []; up = root }
let working_dir = ref root;
type path = string list

let mkdir d = 
  let dir = { name = d; subdirs = []; up = !working_dir } in
  (!working_dir).subdirs <- dir :: (!working_dir).subdirs

let path_to r = 
  let rec path r = 
    if r.up = r then [r.name]
    else List.append (path r.up) [r.name]
  in

  let a : path = path r in
  a

let pwd () = 
  let chemin = 
    let path = path_to !working_dir in
    let rec to_string p = 
      match p with
        | [] -> ""
        | h::d -> h ^ (to_string d)
    in
  to_string path
  in
print_string chemin

let lsR () = 
  let rec ls dir n = 
    let rec print_space n = 
      if n > 0 then
        begin 
          print_string "  ";
          print_space (n-1)
        end
    in
    print_space n;
    Printf.printf "%s\n" dir.name;
    List.iter (fun a -> ls a (n+1)) dir.subdirs
  in
ls (!working_dir) 0

let find nom l = 
  let rec aux nom l = 
    match l with
      | [] -> raise Not_found
      | h::d -> if h.name = nom then h
                else aux nom d
  in
aux nom l

exception Path_Error

let go_to p = 
  let rec go dir p = 
    match p with
      | [] -> dir
      | h::d -> try
                  let subdir = find h dir.subdirs in
                  go subdir d
                with Not_found -> raise Path_Error
  in
  let tete = List.hd p in
  if tete = "/" then go root (List.tl p)
  else go !working_dir p

let cd p = 
  try
    let dir = go_to p in
    working_dir := dir
  with Path_Error -> raise Path_Error
;;