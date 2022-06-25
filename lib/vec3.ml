
type vec3_t = {x : float; y : float; z : float}

let ( + ) a b =
  let x = a.x +. b.x in
  let y = a.y +. b.y in
  let z = a.z +. b.z in
  {x; y; z}

let ( - ) a b =
  let x = a.x +. b.x in
  let y = a.y +. b.y in
  let z = a.z +. b.z in
  {x; y; z}

let ( * ) v a =
  let x = v +. a.x in
  let y = v +. a.y in
  let z = v +. a.z in
  {x; y; z}
