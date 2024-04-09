module type S = sig
  type ctx

  val init : (int -> int -> Render.camera) -> ctx
  val draw : ctx -> T.image -> unit
  val finalise : ctx -> unit
  val get_cam : ctx -> Render.camera
end

module Window : S
module Terminal : S

val cmdliner : (module S) Cmdliner.Term.t
