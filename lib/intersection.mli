
open Vec3

type ray_t = {o : vec3_t; d : vec3_t}

type torus_t = {maj_r : float; min_r : float}

val find_intersection : ray_t -> torus_t -> vec3_t option

val point_along_ray : ray_t -> float -> vec3_t

val string_of_ray : ray_t -> string

val print_ray : ray_t -> unit
