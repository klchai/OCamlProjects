type marque = { c : bool; p: int * int }
type damier = marque list

let l = [ { p = 2,0; c=true};
	  { p = 1,0; c=true};
	  { p = 1,1; c=false};
	  { p = 0,0; c=true};
	  { p = 0,2; c=false}; ]

let dans_les_bornes l =
  List.for_all
    (fun { p = (x, y) } -> 0 <= x && x <= 2  &&  0 <= y && y <= 2) l

let existe_symbole l i j = List.exists (fun { p = (x, y) } -> x=i && y = j) l

let rec sans_doublons l =
  match l with
  | [] -> true
  | { p = (x, y) } :: s ->
    not (existe_symbole s x y) && sans_doublons s

let compter l =
  List.fold_left (fun (x, o) { c = b } ->
    if b then (x + 1, o) else (x, o + 1)) (0, 0) l

let bonne_grille l =
  let (cr, ce) = compter l in
   sans_doublons l && dans_les_bornes l  &&  (cr = ce || cr = ce + 1)

exception Grille_invalide

let bonne_grille_exn l =
  if not (bonne_grille l) then raise Grille_invalide

let gagne l =
  let m = existe_symbole l in
  let horiz i = m i 0 && m i 1 && m i 2 in
  let vert j = m 0 j && m 1 j && m 2 j in
  let diag1 = m 0 0 && m 1 1 && m 2 2 in
  let diag2 = m 0 2 && m 1 1 && m 2 0 in
  horiz 0 || horiz 1 ||  horiz 2 ||
  vert 0 ||  vert 1 ||  vert 2 || diag1 || diag2

let extraire l s = List.filter (fun m -> m.c = s) l

let qui_gagne l =
  let msg =
    try
      bonne_grille_exn l;
      let croix = extraire l true in
      let ronds = extraire l false in
      if gagne croix then "les croix gagnent"
      else if gagne ronds then  "les ronds gagnent"
      else "partie nulle"
    with | Grille_invalide -> "Grille invalide"
  in
  Printf.printf "%s\n" msg


let jeu = [
	  { p = 0,0; c=true};
	  { p = 0,1; c=true};
	  { p = 0,2; c=false};
	  { p = 1,0; c=true};
	  { p = 1,1; c=false};
	  { p = 1,2; c=false}; ]

let test = qui_gagne jeu
