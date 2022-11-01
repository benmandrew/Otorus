open Otorus

let init_window ~(cam : Render.camera) =
  let open Graphics in
  open_graph "";
  resize_window cam.width cam.height;
  set_color cam.bg_color;
  fill_rect 0 0 cam.width cam.height

(* let exit_handler (status : Graphics.status) =
   if status.keypressed && status.key == ' ' then raise Exit else () *)

let () =
  (* let start = Unix.gettimeofday () in *)
  init_window ~cam:Config.cam;

  Engine.set_render_dims_xy
    ~x:(int_of_string Sys.argv.(1))
    ~y:(int_of_string Sys.argv.(2));
  Engine.ParallelRow.render ~cam:Config.cam Config.tori;

  (* let stop = Unix.gettimeofday () in *)
  (* Printf.printf "Execution time: %fs\n%!" (stop -. start); *)
  (* Graphics.loop_at_exit [ Key_pressed ] exit_handler *)
  Graphics.close_graph ()
