open Linalg

type t

val create : float -> float -> t
val intersection : t -> Ray.t -> Vec3.t option
val normal : t -> Vec3.t -> Vec3.t
