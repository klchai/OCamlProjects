(** Fils Imperative *)
type 'a cell = { elt : 'a; mutable next : 'a cell }
type 'a t = (('a cell) option) ref

let create() = ref None

let is_Empty q = 
  !q = None

let push x q = 
  (* Si la liste est nulle *)
  match !q with
    (* On creer premiere cellule *)
    | None -> let rec c = { elt = x; next = c } in
      q := Some c
    | Some last -> let c = { elt = x; next = last.next } in
      last.next <- c;
      q := Some c

let pop q = 
    match !q with
      | None -> invalid_arg "pop"
      | Some last when last.next == last -> q := None; last.elt
      | Some last -> let first = last.next in
        last.next <- first.next; first.elt

(** Files persistantes *)
type 'a t = 'a list * 'a list
let empty = [], []
let is_empty l = 
    match l with
      | [], [] -> true
      | _ -> false

let push x (o, i) = (o, x :: i)

let pop = function
    | [], [] -> invalid_arg "pop"
    | x :: o, i -> x, (o, i)
    | [], i ->
      match List.rev i with
        | x :: o -> x, (o, [])
        | [] -> assert false

(* let f = ([1;2;3],[5;4])
   pop f-> (1,[2;3],[5;4])
*)

(* La structure de tas (heap，堆) *)
type 'a t = Empty | Node of 'a t * 'a * 'a t

let empty = Empty

let is_empty h = h = Empty

let get_min = function
  | Empty -> invalid_arg "get_min"
  | Node (_, x, _) -> x

let rec merge ha hb = 
  match ha, hb with
    | Empty, h 
    | h, Empty -> h
    | Node (la, xa, ra), Node (lb, xb, rb) -> 
        if  xa <= xb then
          Node (ra, xa, merge la hb)
        else Node (rb, xb, merge lb ha)

let add x h = merge (Node (Empty, x, Empty)) h

let remove_min = function
  | Empty -> invalid_arg "remove_min"
  | Node (a, _, b) -> merge a b