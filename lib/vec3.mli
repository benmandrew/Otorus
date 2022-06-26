
type vec3_t = {x : float; y : float; z : float}

val ( ++ ) : vec3_t -> vec3_t -> vec3_t

val ( -- ) : vec3_t -> vec3_t -> vec3_t

val ( ** ) : float -> vec3_t -> vec3_t

val magnitude : vec3_t -> float

val normalised : vec3_t -> vec3_t

val string_of_vec3 : vec3_t -> string

val print_vec3 : vec3_t -> unit
