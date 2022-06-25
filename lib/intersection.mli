
type vec3_t = {x : float; y : float; z : float}

type ray_t = {o : vec3_t; d : vec3_t}

type torus_t = {maj_r : float; min_r : float}

val find_intersection : ray_t -> torus_t -> float

val point_along_ray : ray_t -> float -> vec3_t

