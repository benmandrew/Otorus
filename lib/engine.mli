module type ENGINE = sig
  val set_render_dims_xy : x:int -> y:int -> unit
  val set_render_dims_y : y:int -> unit

  val render :
    cam:Render.camera -> draw:(T.image -> unit) -> Torus.t list -> unit
end

module ParallelTile : ENGINE
module ParallelRowAuto : ENGINE
module ParallelColumnAuto : ENGINE
module SequentialTile : ENGINE
