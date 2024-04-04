type image = (int * int * int) array array

module type ENGINE = sig
  type nonrec image = image

  val set_render_dims_xy : x:int -> y:int -> unit
  val set_render_dims_y : y:int -> unit
  val render : draw:(image -> unit) -> cam:Render.camera -> Torus.t list -> unit
end

module ParallelTile : ENGINE
module ParallelRowAuto : ENGINE
module ParallelColumnAuto : ENGINE
module SequentialTile : ENGINE
