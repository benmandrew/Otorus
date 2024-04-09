open Otorus
(* module Frontend = Frontend.Terminal
   module Engine = Engine.ParallelRowAuto *)

let fps = 30

let rotate i (tori : Torus.t list) =
  let d = float_of_int i /. 10. in
  let f (t : Torus.t) =
    let trf = Torus.transform t in
    let trf = { trf with psi = trf.psi +. d } in
    let trf = { trf with phi = trf.phi +. (d *. 0.33) } in
    let trf = { trf with theta = trf.theta -. (d *. 0.1) } in
    Torus.create ~maj_r:(Torus.maj_r t) ~min_r:(Torus.min_r t) trf
      (Torus.color t)
  in
  List.map f tori

let main (module Frontend : Frontend.S) (module Engine : Engine.S) =
  let ctx = Frontend.init Config.gen_cam in
  let cam = Frontend.get_cam ctx in
  let draw = Frontend.draw ctx in
  for i = 0 to 400 do
    Engine.render ~cam ~draw @@ rotate i Config.tori;
    Unix.sleepf (1. /. float_of_int fps)
  done;
  Frontend.finalise ctx

open Cmdliner

let cmd =
  let doc =
    "Render tori in parallel, drawing in a window or in the terminal."
  in
  let info = Cmd.info "otorus" ~doc in
  Cmd.v info Term.(const main $ Frontend.cmdliner $ Engine.cmdliner)

let () = exit @@ Cmd.eval cmd
