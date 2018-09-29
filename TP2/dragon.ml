let rec dragon n x y z t =
  if n = 1 then
    (Graphics.moveto (int_of_float x) (int_of_float y);
     Graphics.lineto (int_of_float z) (int_of_float t))
  else
    let u = (x+.z)/.2. +. (t-.y)/.2. in
    let v = (y+.t)/.2. -. (z-.x)/.2. in
    (dragon (n-1) x y u v; dragon (n-1) z t u v)

let main () = 
  Graphics.open_graph " 400x400";
  dragon 20 20. 20. 220. 220.;
  ignore (Graphics.wait_next_event [Graphics.Button_down])

let () = main ()