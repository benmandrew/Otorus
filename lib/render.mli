open Linalg

val compute_ray : int -> int -> int -> int -> float -> Mat3.t -> Ray.t
val render_ray : Ray.t -> Torus.t -> Graphics.color option
