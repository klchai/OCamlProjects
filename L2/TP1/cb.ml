(**Le Cadre*)
(* Les valeurs du cadre *)
let left = 0.
let right = 300.
let down = 0.
let up = 500.
(* Ouvre une fenetre graphique*)
(**La balle*)
let ball = 5
(* Affiche la balle comme un cercle de couleur noire *)
let draw_ball x y = Graphics.fill_circle (int_of_float x)(int_of_float y) ball
(**Mouvement de la balle*)

(**La raquette*)
let paddle = 50
let thick = 4
(* Dessine la raquette *)
let draw_paddle x = Graphics.fill_rect x 0 paddle thick
(* Position de la souris *)
let position_paddle () = 
  let x = fst(Graphics.mouse_pos ()) in
  max 0(min x ((int_of_float right)-paddle))
(**Rebonds*)
(* Renvoie le noueveau vecteur vitesse de la balle *)
let new_position (x,y)(vx,vy)= x +. vx, y +. vy

let bounce (x,y)(vx,vy) xr =
  let vx = if x <= left || x >= right then -. vx else vx in
  let vy = 
      if y <= float thick
      && x >= (float xr) && x<=(float xr +. (float paddle)) || y >= up then -. vy
      else vy in
  (vx,vy)
(**Boucle principale du jeu*)
let rec jeu (x,y)(vx,vy) = 
  Graphics.clear_graph ();
  draw_ball x y;
  if y <= down then failwith "perdu";
  let xr = position_paddle() in
  draw_paddle xr;
  Graphics.synchronize ();
  let vx,vy = bounce (x,y)(vx,vy) xr in
  let x', y' = new_position (x,y)(vx,vy) in
  jeu (x',y')(vx,vy)

let () = 
  let s = Printf.sprintf" %dx%d"(int_of_float right)(int_of_float up) in
  Graphics.open_graph s;
  Graphics.auto_synchronize false;
  jeu(150., float thick)(0.05,0.05)

