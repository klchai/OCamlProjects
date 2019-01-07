type t = { words : string list ; branches : (int*t) list }

let empty = { words = [] ; branches = [] }

let find list tree =
  let rec aux l t =
    match l with
    | [] -> t.words
    | hd::tl -> aux tl (List.assoc hd t.branches)
  in
  try aux list tree
  with Not_found -> []

let rec change_assoc key elt list =
  match list with
  | [] -> [(key, elt)]
  | (k, _)::tl when k = key -> (k, elt)::tl
  | hd::tl -> hd::(change_assoc key elt tl)

let rec add list word tree =
  let rec add_word words acc =
    match words with
    | [] -> word::acc
    | hd::tl when hd = word -> acc@words
    | hd::tl -> add_word tl (hd::acc)
  in
  match list with
  | [] -> { tree with words = add_word tree.words [] }
  | hd::tl ->
    let branch =
      try List.assoc hd tree.branches
      with Not_found -> empty
    in
    let new_branch = add tl word branch in
    { tree with branches = change_assoc hd new_branch tree.branches }

exception Found of int list
let find_word word tree =
  let rec aux seq t =
    if List.mem word t.words then
      raise (Found (List.rev seq))
    else
      List.iter
        (fun (k, t) -> aux (k::seq) t)
        t.branches
  in
  try aux [] tree ; raise Not_found
  with Found s -> s
