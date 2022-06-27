open Linalg

type t = { maj_r : float; min_r : float }

val intersection : t -> Ray.t -> Vec3.t option
val normal : t -> Vec3.t -> Vec3.t
