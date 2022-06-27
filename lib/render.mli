val compute_ray :
  int -> int -> int -> int -> float -> Vec3.mat3_t -> Intersection.ray_t

val render_ray :
  Intersection.ray_t -> Intersection.torus_t -> Graphics.color option
