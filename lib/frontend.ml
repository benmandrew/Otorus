module type FRONTEND = sig
  val init : Render.camera -> unit
  val draw : (int * int * int) array array -> unit
  val finalise : unit -> unit
end

module Graphics : FRONTEND = struct
  let init cam =
    Graphics.open_graph (Printf.sprintf " %dx%d" cam.Render.width cam.height);
    Graphics.(set_color (rgb 0 0 0));
    Graphics.fill_rect 0 0 cam.width cam.height

  let draw img =
    let img = Array.map (Array.map (fun (r, g, b) -> Graphics.rgb r g b)) img in
    Graphics.(draw_image (make_image img) 0 0)

  let exit_handler (status : Graphics.status) =
    if status.keypressed && status.key == ' ' then raise Exit else ()

  let finalise () = Graphics.loop_at_exit [ Key_pressed ] exit_handler
end

module Notty : FRONTEND = struct
  let init _ = ()
  let draw _ = ()
  let finalise () = ()
end
