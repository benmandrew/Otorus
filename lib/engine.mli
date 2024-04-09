module type S = sig
  val set_render_dims_xy : x:int -> y:int -> unit
  val set_render_dims_y : y:int -> unit

  val render :
    cam:Render.camera -> draw:(T.image -> unit) -> Torus.t list -> unit
end

module ParallelTile : S
module ParallelRowAuto : S
module ParallelColumnAuto : S
module SequentialTile : S

val cmdliner : (module S) Cmdliner.Term.t
