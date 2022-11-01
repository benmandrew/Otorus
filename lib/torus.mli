open Linalg

type t

val create : maj_r:float -> min_r:float -> Transform.t -> Vec.t -> t
val intersection : t -> Ray.t -> (Vec.t * float) option
val normal : t -> Vec.t -> Vec.t
val color : t -> Vec.t
