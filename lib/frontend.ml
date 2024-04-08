module type FRONTEND = sig
  type ctx

  val init : Render.camera -> ctx
  val draw : ctx -> T.image -> unit
  val finalise : ctx -> unit
end

module Window : FRONTEND = struct
  type ctx = unit

  let init cam =
    Graphics.open_graph (Printf.sprintf " %dx%d" cam.Render.width cam.height);
    Graphics.(set_color (rgb 0 0 0));
    Graphics.fill_rect 0 0 cam.width cam.height

  let draw () img =
    let img =
      Array.map (Array.map (fun (r, g, b, _a) -> Graphics.rgb r g b)) img
    in
    Graphics.(draw_image (make_image img) 0 0)

  let exit_handler (status : Graphics.status) =
    if status.keypressed && status.key == ' ' then raise Exit else ()

  let finalise () = Graphics.loop_at_exit [ Key_pressed ] exit_handler
end

module Terminal : FRONTEND = struct
  open Notty
  open Notty_unix

  type ctx = Term.t

  let init _ = Term.create ()

  (* Luminance *)
  let lum r g b =
    let r = float_of_int r in
    let g = float_of_int g in
    let b = float_of_int b in
    (0.2126 *. r) +. (0.7152 *. g) +. (0.0722 *. b)

  let get_color r g b = A.(fg (rgb_888 ~r ~g ~b))

  let pixel_ascii_map =
    "`^\",:;Il!i~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"

  let get_char r g b =
    let i =
      lum r g b *. (float_of_int @@ String.length pixel_ascii_map) /. 255.
    in
    String.get pixel_ascii_map @@ int_of_float i

  let render (w, h) img : image =
    let f x y =
      let r, g, b, a = img.(y).(x) in
      if a = 0 then I.void 1 1
      else I.char (get_color r g b) (get_char r g b) 1 1
    in
    I.tabulate w (h - 1) f

  let draw ctx img =
    let size = Term.size ctx in
    Term.image ctx @@ render size img

  let finalise _ = Unix.sleepf 1.0
end
