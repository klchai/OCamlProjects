open Keypad_gui

let print_words words =
  List.iter print_endline words ;
  print_endline "----------------------"

let print_keyseq keyseq =
  List.iter print_int keyseq ;
  print_endline ""

let key_of_char c =
  match c with
  | 'a' | 'â' | 'à' | 'ä' | 'b' | 'c' | 'ç' | '-' -> 2
  | 'd' | 'e' | 'é' | 'è' | 'ê' | 'ë' | 'f' -> 3
  | 'g' | 'h' | 'i' | 'ï' -> 4
  | 'j' | 'k' | 'l' -> 5
  | 'm' | 'n' | 'o' | 'ô' | 'ö' -> 6
  | 'p' | 'q' | 'r' | 's' -> 7
  | 't' | 'u' | 'ù' | 'ü' | 'v' -> 8
  | 'w' | 'x' | 'y' | 'z' -> 9
  | _ -> print_char c ; assert false

let keyseq_of_word word =
  let rec aux acc cpt sz word =
    if cpt >= sz then
      acc
    else
      let key =
        try key_of_char word.[cpt]
        with exn -> print_endline word ; raise exn
      in
      aux (key::acc) (cpt+1) sz word
  in
  List.rev (aux [] 0 (String.length word) (String.lowercase_ascii word))

let init () =
  let rec aux file tree =
    let word =
      try Some (input_line file)
      with End_of_file -> None
    in
    match word with
    | None -> tree
    | Some word ->
      aux file (T9.add (keyseq_of_word word) word tree)
  in
  let file = open_in "dico.txt" in
  let t9 = aux file T9.empty in
  close_in file ;
  t9

let rec interact t9 seq =
  let key = clic () in
  let seq =
    if key = 1 then List.tl seq
    else key::seq
  in
  let words = T9.find (List.rev seq) t9 in
  print_keyseq (List.rev seq) ;
  print_words words ;
  interact t9 seq

let () =
  let t9 = init () in
  show () ;
  interact t9 []
