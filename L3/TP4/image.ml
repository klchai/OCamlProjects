open Graphics
   
type 'a t = F of 'a | N of 'a t * 'a t * 'a t * 'a t

let dessine k img =
  let rec aux i j k = function
    | F c ->
       set_color c; 
       fill_rect i j k k
    | N(a, b, c, d) -> 
	let k = k / 2 in
	let ik = i + k in
	let jk = j + k in
	aux i j k a;
	aux ik j k b;
	aux i jk k c;
	aux ik jk k d
  in
  aux 0 0 k img

let affiche img =
  open_graph " 512x512";
  dessine 512 img;
  ignore (read_key ())
