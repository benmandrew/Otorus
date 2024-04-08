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
    Torus.create ~maj_r:(Torus.maj_r t) ~min_r:(Torus.min_r t) trf
      (Torus.color t)
  in
  List.map f tori

let () =
  let ctx = Frontend.init Config.cam in
  if Array.length Sys.argv > 1 then
    Engine.set_render_dims_xy
      ~x:(int_of_string Sys.argv.(1))
      ~y:(int_of_string Sys.argv.(2));
  for i = 0 to 200 do
    Engine.render ~draw:(Frontend.draw ctx) ~cam:Config.cam
    @@ rotate i Config.tori;
    Unix.sleepf (1. /. float_of_int fps)
  done;
  Frontend.finalise ctx
