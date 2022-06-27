(* open Torus.Intersection
open Torus.Vec3 *)

open Torus

let width = 960
let height = 640
let torus : Intersection.torus_t = {maj_r = 5.0; min_r = 1.0}
let rot_mat : Vec3.mat3_t = Vec3.rot_from_euler (Float.pi /. 4.0) (Float.pi /. 4.0) (Float.pi /. 4.0)

(* open Graphics *)

let exit_handler (status : Graphics.status) =
  if status.keypressed && status.key == ' ' then raise Exit
  else ()

let init_window () =
  Graphics.open_graph "";
  Graphics.resize_window width height;
  Graphics.set_color (Graphics.rgb 220 220 220);
  Graphics.fill_rect 0 0 width height

let () =
  init_window ();
  for y = 0 to height do
    for x = 0 to width do
      Graphics.moveto x y;
      let ray = Render.compute_ray x y width height 90.0 rot_mat in
      match Render.render_ray ray torus with
      | None -> ()
      | Some c -> begin
        Graphics.set_color c;
        Graphics.plot x y
      end
    done
  done;
  Graphics.loop_at_exit [Key_pressed] exit_handler




