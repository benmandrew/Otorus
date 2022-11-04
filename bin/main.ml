open Otorus

let init_window ~(cam : Render.camera) =
  let open Graphics in
  open_graph (Printf.sprintf " %dx%d" cam.width cam.height);
  set_color (Graphics.rgb 0 0 0);
  fill_rect 0 0 cam.width cam.height

let exit_handler (status : Graphics.status) =
  if status.keypressed && status.key == ' ' then raise Exit else ()

let () =
  (* let start = Unix.gettimeofday () in *)
  init_window ~cam:Config.cam;
  begin
    if Array.length Sys.argv > 1 then
      Engine.set_render_dims_xy
        ~x:(int_of_string Sys.argv.(1))
        ~y:(int_of_string Sys.argv.(2))
    else ()
  end;

  Engine.SequentialTile.render ~cam:Config.cam Config.tori;

  (* let stop = Unix.gettimeofday () in *)
  (* Printf.printf "Execution time: %fs\n%!" (stop -. start); *)
  Graphics.loop_at_exit [ Key_pressed ] exit_handler
