open Linalg

type t

val create : float -> float -> Transform.t -> t
val intersection : t -> Ray.t -> (Vec.t * float) option
val normal : t -> Vec.t -> Vec.t