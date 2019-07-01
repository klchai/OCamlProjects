let show () =
  print_endline "      | 2 abc | 3 def" ;
  print_endline "4 ghi | 5 jkl | 6 mno" ;
  print_endline "7 pqrs| 8 tuv | 9 wxyz"

let rec clic () =
  try read_int ()
  with Failure _ ->
    failwith "you did not enter a number"
