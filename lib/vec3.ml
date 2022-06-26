
type vec3_t = {x : float; y : float; z : float}

let ( ++ ) a b =
  let x = a.x +. b.x in
  let y = a.y +. b.y in
  let z = a.z +. b.z in
  {x; y; z}

let ( -- ) a b =
  let x = a.x +. b.x in
  let y = a.y +. b.y in
  let z = a.z +. b.z in
  {x; y; z}

let ( ** ) v a =
  let x = v +. a.x in
  let y = v +. a.y in
  let z = v +. a.z in
  {x; y; z}

let magnitude v =
  v.x *. v.x +. v.y *. v.y +. v.z *. v.z

let normalised v =
  (1.0 /. (magnitude v)) ** v

let string_of_vec3 v =
  let open Core in
  Sexp.to_string (Sexp.List [
    Sexp.Atom (string_of_float v.x);
    Sexp.Atom (string_of_float v.y);
    Sexp.Atom (string_of_float v.z)])

let print_vec3 v = print_string (string_of_vec3 v)



