module type FRONTEND = sig
  type ctx

  val init : Render.camera -> ctx
  val draw : ctx -> T.image -> unit
  val finalise : ctx -> unit
end

module Window : FRONTEND
module Terminal : FRONTEND
