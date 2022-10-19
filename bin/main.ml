open Otorus
open Otorus.Linalg

let width = 960
let height = 640
let pos = Vec.make_point 0.0 0.0 (-25.0)
let field_of_view = 60.0

let tori =
  let open Transform in
  [
    Torus.create 5.0 1.0
      {
        x = 3.5;
        y = 0.0;
        z = 0.0;
        psi = -.(Float.pi /. 8.0);
        theta = 0.0;
        phi = -.(Float.pi /. 16.0);
      }
      (Vec.make_vec 1.0 0.0 0.0);
    Torus.create 5.5 0.8
      {
        x = -2.5;
        y = 0.0;
        z = 0.0;
        psi = Float.pi /. 4.0;
        theta = 0.0;
        phi = Float.pi /. 4.0;
      }
      (Vec.make_vec 0.0 1.0 0.0);
  ]

let bg_value = 225

let exit_handler (status : Graphics.status) =
  if status.keypressed && status.key == ' ' then raise Exit else ()

let init_window () =
  let open Graphics in
  open_graph "";
  resize_window width height;
  set_color (rgb bg_value bg_value bg_value);
  fill_rect 0 0 width height

let render_pixel x y =
  Graphics.moveto x y;
  let ray = Render.compute_ray x y width height pos field_of_view in
  match Render.render_ray ray tori with
  | None ->
      ()
      (* Graphics.plot x y;
      Graphics.set_color Graphics.green; *)
  | Some c ->
      Graphics.set_color c;
      Graphics.plot x y

      
let () =
  let start = Unix.gettimeofday () in
  init_window ();
  for y = 0 to height do
    for x = 0 to width do
      render_pixel x y
    done
  done;
  let stop = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs\n%!" (stop -. start);
  Graphics.loop_at_exit [ Key_pressed ] exit_handler
