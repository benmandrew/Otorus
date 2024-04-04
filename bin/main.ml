open Otorus
module Frontend = Frontend.Graphics
module Engine = Engine.SequentialTile

let () =
  Frontend.init Config.cam;
  if Array.length Sys.argv > 1 then
    Engine.set_render_dims_xy
      ~x:(int_of_string Sys.argv.(1))
      ~y:(int_of_string Sys.argv.(2));
  Engine.render ~draw:Frontend.draw ~cam:Config.cam Config.tori;
  Frontend.finalise ()
