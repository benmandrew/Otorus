
type ray_t = {ox : float; oy : float; oz : float; dx : float; dy : float; dz : float}

type torus_t = {maj_r : float; min_r : float}

val find_intersection : ray_t -> torus_t -> float



