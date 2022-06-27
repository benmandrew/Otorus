open Torus

let width = 960
let height = 640
let field_of_view = 90.0
let torus : Intersection.torus_t = { maj_r = 5.0; min_r = 1.0 }
let bg_value = 120

let rot_mat : Vec3.mat3_t =
  Vec3.rot_from_euler (Float.pi /. 4.0) (Float.pi /. 4.0) (Float.pi /. 4.0)

let exit_handler (status : Graphics.status) =
  if status.keypressed && status.key == ' ' then raise Exit else ()

let init_window () =
  let open Graphics in
  open_graph "";
  resize_window width height;
  set_color (rgb bg_value bg_value bg_value);
  fill_rect 0 0 width height

let () =
  init_window ();
  for y = 0 to height do
    for x = 0 to width do
      Graphics.moveto x y;
      let ray = Render.compute_ray x y width height field_of_view rot_mat in
      match Render.render_ray ray torus with
      | None -> ()
      | Some c ->
          Graphics.set_color c;
          Graphics.plot x y
    done
  done;
  Graphics.loop_at_exit [ Key_pressed ] exit_handler
