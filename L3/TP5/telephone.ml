type 'a option = 
  | Some of 'a
  | None

let rec read_file file = 
  try let l = input_line file in
    l::read_file file
  with End_of_file -> []

let rec term_read file acc = 
  let cpt = 
    try Some (input_line file)
    with End_of_file -> None
  in

  match cpt with
    | Some l -> term_read file (l::acc)
    | None -> List.rev acc