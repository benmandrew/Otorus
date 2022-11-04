val set_render_dims_xy : x:int -> y:int -> unit
val set_render_dims_y : y:int -> unit

module type ENGINE = sig
  val render : cam:Render.camera -> Torus.t list -> unit
end

module ParallelTile : ENGINE
module ParallelRowAuto : ENGINE
module ParallelColumnAuto : ENGINE
module SequentialTile : ENGINE
