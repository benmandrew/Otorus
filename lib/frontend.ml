module type FRONTEND = sig
  type ctx

  val init : (int -> int -> Render.camera) -> ctx
  val draw : ctx -> T.image -> unit
  val finalise : ctx -> unit
  val get_cam : ctx -> Render.camera
end

module Window : FRONTEND = struct
  type ctx = Render.camera

  let init gen_cam =
    let width = 960 in
    let height = 640 in
    Graphics.open_graph (Printf.sprintf " %dx%d" width height);
    Graphics.(set_color (rgb 0 0 0));
    Graphics.fill_rect 0 0 width height;
    gen_cam width height

  let draw _ img =
    let img =
      Array.map (Array.map (fun (r, g, b, _a) -> Graphics.rgb r g b)) img
    in
    Graphics.(draw_image (make_image img) 0 0)

  let exit_handler (status : Graphics.status) =
    if status.keypressed && status.key == ' ' then raise Exit else ()

  let finalise _ = Graphics.loop_at_exit [ Key_pressed ] exit_handler
  let get_cam ctx = ctx
end

module Terminal : FRONTEND = struct
  open Notty_unix

  type ctx = Term.t * Render.camera

  let init (gen_cam : int -> int -> Render.camera) =
    let tty = Term.create () in
    let width, height = Term.size tty in
    let cam = gen_cam width height in
    (tty, cam)

  open Notty

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

  let draw (ctx, _) img =
    let size = Term.size ctx in
    Term.image ctx @@ render size img

  let finalise _ = Unix.sleepf 1.0
  let get_cam (_, cam) = cam
end
