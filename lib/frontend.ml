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
  open Notty
  open Notty_unix

  let tty = ref @@ Term.create ()
  let init _ = ()

  let render (w, h) img : image =
    let f x y =
      let r, g, b = img.(y).(x) in
      if r <= 0 && g <= 0 && b <= 0 then I.void 1 1
      else I.char A.(fg (rgb_888 ~r ~g ~b)) 'O' 1 1
    in
    I.tabulate w (h - 1) f

  let draw img =
    let size = Term.size !tty in
    Term.image !tty @@ render size img

  let finalise () = ()
end
