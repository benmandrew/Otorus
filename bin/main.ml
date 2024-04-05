open Otorus
module Frontend = Frontend.Terminal
module Engine = Engine.SequentialTile

let () =
  let ctx = Frontend.init Config.cam in
  if Array.length Sys.argv > 1 then
    Engine.set_render_dims_xy
      ~x:(int_of_string Sys.argv.(1))
      ~y:(int_of_string Sys.argv.(2));
  Engine.render ~draw:(Frontend.draw ctx) ~cam:Config.cam Config.tori;
  Frontend.finalise ctx
