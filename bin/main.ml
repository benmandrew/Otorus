(* open Torus.Intersection
open Torus.Vec3 *)

open Torus

let width = 640
let height = 480
let torus : Intersection.torus_t = {maj_r = 50.0; min_r = 13.0}

(* open Graphics *)

let exit_handler (status : Graphics.status) =
  if status.keypressed && status.key == ' ' then raise Exit
  else ()

let init_window () =
  Graphics.open_graph "";
  Graphics.resize_window width height;
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 width height

let () =
  init_window ();
  for y = 0 to height do
    (* let ry = y - height / 2 in *)
    for x = 0 to width do
      (* let rx = x - width / 2 in *)
      Graphics.moveto x y;
      let ray = Render.compute_ray x y width height 60.0 in
      Intersection.print_ray ray;
      print_newline ();
      match Render.render_ray ray torus with
      | None -> begin
        Graphics.set_color Graphics.green;
        Graphics.plot x y
      end
      | Some c -> begin
        Graphics.set_color c;
        Graphics.plot x y
      end
    done
  done;
  Graphics.loop_at_exit [Key_pressed] exit_handler




