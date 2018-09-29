open Graphics;;

let r = 10

let rec push x y = 
 let event = wait_next_event [Button_down] in
 let mx = event.mouse_x in
 let my = event.mouse_y in
 if (mx - x) * (mx - x) + (my - y) * (my - y)  < r * r then
drag x y
 else push x y

and drag x y = 
 let event = wait_next_event [Mouse_motion; Button_up] in
 if not (event.button) then(
  clear_graph ();
  set_color black;
  fill_circle event.mouse_x event.mouse_y r;
  synchronize ();
  push event.mouse_x event.mouse_y;)
 else(
   let mx = event.mouse_x in 
   let my = event.mouse_y in 
   clear_graph ();
   set_color black;
   fill_circle x y r;
   set_color red;
   fill_circle mx my r;
   synchronize ();
   drag x y
 )

let () =
 auto_synchronize false;
 open_graph " 300x400";
 set_color black;
 fill_circle 100 100 r;
 push 100 100