module type FRONTEND = sig
  val init : Render.camera -> unit
  val draw : T.image -> unit
  val finalise : unit -> unit
end

module Window : FRONTEND
module Terminal : FRONTEND
