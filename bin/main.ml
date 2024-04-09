open Otorus
module Frontend = Frontend.Terminal
module Engine = Engine.ParallelRowAuto

let fps = 30

let rotate i (tori : Torus.t list) =
  let d = float_of_int i /. 7.5 in
  let f (t : Torus.t) =
    let trf = Torus.transform t in
    let trf = { trf with psi = trf.psi +. d } in
    let trf = { trf with phi = trf.phi +. (d *. 0.33) } in
    let trf = { trf with theta = trf.theta -. (d *. 0.1) } in
    Torus.create ~maj_r:(Torus.maj_r t) ~min_r:(Torus.min_r t) trf
      (Torus.color t)
  in
  List.map f tori

let set_dims () =
  if Array.length Sys.argv > 1 then
    Engine.set_render_dims_xy
      ~x:(int_of_string Sys.argv.(1))
      ~y:(int_of_string Sys.argv.(2))

let () =
  set_dims ();
  let ctx = Frontend.init Config.gen_cam in
  let cam = Frontend.get_cam ctx in
  let draw = Frontend.draw ctx in
  for i = 0 to 200 do
    Engine.render ~cam ~draw @@ rotate i Config.tori;
    Unix.sleepf (1. /. float_of_int fps)
  done;
  Frontend.finalise ctx
