open Graphics

type descr =
  { width : int ;
    height : int ;
    column : int ;
    line : int ;
  }

let get_descr () =
  let width = size_x () - 1 in
  let height = size_y () - 1 in
  { width ;
    height ;
    column = width/3 ;
    line = height/4 ;
  }

let draw_grid descr =
  (* lines *)
  for i = 1 to 3 do
    let y = i*descr.line in
    moveto 0 y ;
    lineto descr.width y
  done ;
  for i = 1 to 2 do
    let x = i*descr.column in
    moveto x 0 ;
    lineto x descr.height
  done

let write_num descr =
  let pad_x, pad_y =
    let txt_x, txt_y = text_size "9 wxyz" in
    (descr.column - txt_x)/2, (descr.line - txt_y)/2
  in
  let t = [ [ "DEL"  ; "abc" ; "def"  ] ;
            [ "ghi"  ; "jkl" ; "mno"  ] ;
            [ "pqrs" ; "tuv" ; "wxyz" ] ]
  in
  List.iteri
    (fun y l ->
       List.iteri
         (fun x str ->
            let num = y*3+x+1 in
            let pos_x = x*descr.column + pad_x in
            let pos_y = (3-y)*descr.line + pad_y in
            moveto pos_x pos_y ;
            let txt = Printf.sprintf "%i %s" num str in
            draw_string txt)
         l)
    t ;
  moveto (descr.column+pad_x) pad_y ;
  draw_string "0"

let show () =
  open_graph " 150x200" ;
  let descr = get_descr () in
  draw_grid descr ;
  write_num descr

let clic () =
  let { mouse_x ; mouse_y ; _ } = wait_next_event [ Button_down ] in
  let descr = get_descr () in
  let x = mouse_x/descr.column in
  let y = 3 - mouse_y/descr.line in
  y*3 + x + 1
  (* Printf.printf "x: %i, y: %i" (mouse_x/descr.column) (3-mouse_y/descr.line) ;
   * 1 *)
