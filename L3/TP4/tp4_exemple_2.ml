open Graphics
open Image

(* 

Placer ici votre code des exercices 1 et 2

*)


(* Il n'est pas nécessaire de comprendre comment les fonctions
   suivantes fonctionnent pour faire le TP. Elles sont fournis
   pour pouvoir effectuer des tests (à la fin du fichier) *)

let dump_tree =
  let rec dump_tree file = function
    | F c ->
       let () = output_char file 'F' in
       output_string file (string_of_int c)
    | N (a,b,c,d) ->
       let () = output_char file 'N' in
       List.iter (dump_tree file) [a;b;c;d] in
  let dump_tree file len q =
    let () = output_string file (string_of_int len) in
    dump_tree file q in
  let dump_tree filename len q =
    let ofile = open_out_bin filename in
    let () = try dump_tree ofile len q
             with e -> let () = close_out ofile in raise e in
    close_out ofile in
  dump_tree

let read_tree =
  let oinput file = try Some (input_char file) with End_of_file -> None in
  let rec read_int file sign acc c =
    match c with
    | '0' .. '9' ->
       let u = Char.code c - Char.code '0' in
       let acc = 10 * acc + sign * u in
       begin match oinput file with
       | Some x -> read_int file sign acc x
       | None -> None , acc
       end
    | c -> Some c , acc in
  let fail_parse () = failwith "failed to parse tree from file" in
  let call_on_next file f = match oinput file with
    | Some x -> f x
    | None -> fail_parse () in
  let read_int file c = match c with
    | '-' -> call_on_next file (read_int file (-1) 0)
    | _ -> read_int file 1 0 c in
  let rec read_tree file c =
    match c with
    | 'F' -> call_on_next file (fun c ->
      let oc , i0 = read_int file c in oc , F i0)
    | 'N' -> let oc = ref (oinput file) in
      let l = ref [] in
      let () = for i = 0 to 3 do
        match !oc with
        | Some c -> let oc' , t = read_tree file c in
          let () = oc := oc' in
          l := t :: !l
        | None -> fail_parse ()
      done in
      begin match !l with
      | [d;c;b;a] -> !oc , N(a,b,c,d)
      | _ -> assert false
      end
    | _ -> fail_parse () in
  let read_tree file =
    call_on_next file (fun c ->
      let oc , len = read_int file c in
      match oc with
      | Some c -> let _ , t = read_tree file c in
        len , t
      | None -> fail_parse ()) in
  let read_tree filename =
    let ifile = open_in_bin filename in
    let compressed = try read_tree ifile
      with e -> let () = close_in ifile in raise e in
    let () = close_in ifile in compressed in
  read_tree

let show scale matrix =
  let len = Array.length matrix in
  let rlen = len * scale in
  let s = string_of_int rlen in
  let wstring = " " ^ s ^ "x" ^ s in
  let () = open_graph wstring in
  let matrix =
    Array.init rlen
      (fun y -> Array.init rlen
                  (fun x ->
                    matrix.(x/scale).(len - 1 - y/scale))) in
  let () = draw_image (make_image matrix) 0 0 in
  ignore(Graphics.read_key());
  close_graph ()

let len , tree = read_tree "mandelbrot.quadtree"


(* il s'agit de *VOS* fonctions image_matrix_of_tree et compress *)
               
let matrix = image_matrix_of_tree len tree
let tree = compress matrix
let () = show 1 (image_matrix_of_tree len tree)

