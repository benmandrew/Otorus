open Linalg

val compute_ray : int -> int -> int -> int -> Vec.t -> float -> Ray.t
val render_ray : Ray.t -> Torus.t list -> Graphics.color option
