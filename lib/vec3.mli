
type vec3_t = {x : float; y : float; z : float}

type mat3_t = {
  x0 : float; x1 : float; x2 : float;
  y0 : float; y1 : float; y2 : float;
  z0 : float; z1 : float; z2 : float}

val vec_add : vec3_t -> vec3_t -> vec3_t

val vec_sub : vec3_t -> vec3_t -> vec3_t

val vec_mul : float -> vec3_t -> vec3_t

val vec_dot : vec3_t -> vec3_t -> float

val magnitude : vec3_t -> float

val normalised : vec3_t -> vec3_t

val mat_mul : mat3_t -> mat3_t -> mat3_t

val mat_vec_mul : mat3_t -> vec3_t -> vec3_t

val rot_from_euler : float -> float -> float -> mat3_t

val string_of_vec3 : vec3_t -> string

val print_vec3 : vec3_t -> unit
