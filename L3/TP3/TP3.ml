(* @author Kelun Chai *)
(* chaikelun@gmail.com *)
type marque = { c : bool; p : int*int}
type grille = marque list

(** Ex1 *)
(* Ex1.1 *)
let g1 = [{c = true; p = (0,0)};
          {c = true; p = (0,1)};
          {c = true; p = (0,2)};
          {c = false; p = (1,1)};
          {c = false; p = (2,0)};
          {c = false; p = (2,2)}]
;;
let g2 = [{c = true; p = (0,0)};
          {c = true; p = (0,1)};
          {c = true; p = (0,2)};
          {c = false; p = (1,1)};
          {c = false; p = (2,0)};
          {c = false; p = (2,2)}]
;;


(* Ex 1.2 *)
let dans_le_bornes size = 
    List.for_all 
      (fun m ->
        let row = fst m.p in
        let col = snd m.p in 
        row >= 0 && row < size && col >= 0 && col < size)
    g1
;;
let dans_le_bornes_2 size = 
    List.for_all
      (fun m ->
        let row,col = m.p in
        row >= 0 && row < size && col >= 0 && col < size)
    g1
;;
let dans_le_bornes_3 size = 
    List.for_all
      (fun { p; _ } ->
        let row,col = p in
        row >= 0 && row < size && col >= 0 && col < size)
    g1
;;
let dans_le_bornes_4 size = 
    List.for_all
      (fun { p = (row,col); _ } -> 
      row >= 0 && row < size && col >= 0 && col < size)
    g1
;;

(* Ex 1.3 *)
let existe_symbole g i j = 
    List.exists
      (fun m ->
        let row = fst m.p in 
        let col = snd m.p in 
        i = row && j = col)
    g
;;
let existe_symbole_2 g i j = 
    List.exists
      (fun m ->
        let row,col = m.p in 
        i = row && j=col)
    g
;;
let existe_symbole_3  g i j = 
    List.exists
      (fun { p; _ } ->
        let row,col = p in 
        i = row && j = col)
    g
;;
let existe_symbole_4 g i j = 
    List.exists
      (fun { p = (row,col); _ } -> i = row && j = col)
    g
;;

(* Ex 1.4 *)
let rec sans_doublons l = 
    match l with
     | [] -> true 
     | x::s -> 
        let row = fst x.p in 
        let col = snd x.p in
        not (existe_symbole l row col) && sans_doublons l
;;
let rec sans_doublons grid = 
    match grid with
      | [] -> true
      | { p = (row,col); _ } :: rest -> 
        not (existe_symbole rest row col) && sans_doublons rest
;;

(* Ex 1.5 *)
let compter grid = 
    List.fold_left
      (fun (x,o) mark ->
        if mark.c then (x+1,o) else (x,o+1))
        (0,0) grid
;;
(* Ex 1.6 *)
let bonne_grille grid = 
    dans_le_bornes 3 && sans_doublons grid &&  
    let (x,o) = compter grid in 
    x-o = 1 || x-o = 0
;;
exception Invalid_grid

(* Ex 1.7 *)
let bonne_grille_exn grid = 
    if not(bonne_grille grid) then raise Invalid_grid
;;
(* Ex 1.8 alter 
   without using the method as
   demanded *)
let list_init f n = 
    let rec aux acc n = 
      if n < 0 then acc
      else aux (f n :: acc)(n-1)
    in 
aux [] (n-1)
;;
(* 所有可能赢的情况 *)
let all_winning_grid n = 
    list_init (fun i -> i,i) n
    :: list_init (fun i -> i,n-i-1) n
    :: (list_init (fun i -> list_init (fun j -> i,j) n) n
     @  list_init (fun i -> list_init (fun j -> j,i) n) n)
;;
let winning_grid grid = 
    List.exists
      (fun list ->
        List.for_all
          (fun (row,col) -> existe_symbole grid row col)
           list)
    (all_winning_grid 3)
;;
(* Ex 1.9 *)
let extraire grid s = List.filter (fun { c; _ } -> c = s) grid;;

(* Ex 1.10 *)
let qui_gagne grid = 
    try
      bonne_grille_exn grid;
      if winning_grid (extraire grid true) then print_endline "X win"
      else if winning_grid (extraire grid false) then print_endline "O win"
      else print_endline "Nul..."
    with
      Invalid_grid -> print_endline "Invalid_grid"