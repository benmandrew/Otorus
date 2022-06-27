
type vec3_t = {x : float; y : float; z : float}

type mat3_t = {
  x0 : float; x1 : float; x2 : float;
  y0 : float; y1 : float; y2 : float;
  z0 : float; z1 : float; z2 : float}

let vec_add a b =
  let x = a.x +. b.x in
  let y = a.y +. b.y in
  let z = a.z +. b.z in
  {x; y; z}

let vec_sub a b =
  let x = a.x -. b.x in
  let y = a.y -. b.y in
  let z = a.z -. b.z in
  {x; y; z}

let vec_mul v a =
  let x = v *. a.x in
  let y = v *. a.y in
  let z = v *. a.z in
  {x; y; z}

let vec_dot v0 v1 =
  v0.x *. v1.x +. v0.y *. v1.y +. v0.z *. v1.z

let magnitude v =
  Float.sqrt (v.x *. v.x +. v.y *. v.y +. v.z *. v.z)

let normalised v =
  vec_mul (1.0 /. (magnitude v)) v

let mat_mul a b =
  {x0 = a.x0 *. b.x0 +. a.y0 *. b.x1 +. a.z0 *. b.x2;
   x1 = a.x1 *. b.x0 +. a.y1 *. b.x1 +. a.z1 *. b.x2;
   x2 = a.x2 *. b.x0 +. a.y2 *. b.x1 +. a.z2 *. b.x2;
   y0 = a.x0 *. b.y0 +. a.y0 *. b.y1 +. a.z0 *. b.y2;
   y1 = a.x1 *. b.y0 +. a.y1 *. b.y1 +. a.z1 *. b.y2;
   y2 = a.x2 *. b.y0 +. a.y2 *. b.y1 +. a.z2 *. b.y2;
   z0 = a.x0 *. b.z0 +. a.y0 *. b.z1 +. a.z0 *. b.z2;
   z1 = a.x1 *. b.z0 +. a.y1 *. b.z1 +. a.z1 *. b.z2;
   z2 = a.x2 *. b.z0 +. a.y2 *. b.z1 +. a.z2 *. b.z2}

let mat_vec_mul a v =
  {x=a.x0 *. v.x +. a.x1 *. v.y +. a.x2 *. v.z;
   y=a.y0 *. v.x +. a.y1 *. v.y +. a.y2 *. v.z;
   z=a.z0 *. v.x +. a.z1 *. v.y +. a.z2 *. v.z}

let rot_from_euler psi theta phi =
  let open Float in
  let r_x = {
    x0=1.0; x1=0.0; x2=0.0;
    y0=0.0; y1=cos psi; y2=(-.sin psi);
    z0=0.0; z1=sin psi; z2=cos psi} in
  let r_y = {
    x0=cos theta; x1=0.0; x2=sin theta;
    y0=0.0; y1=1.0; y2=0.0;
    z0=(-.sin theta); z1=0.0; z2=cos theta} in
  let r_z = {
    x0=cos phi; x1=(-.sin phi); x2=0.0;
    y0=sin phi; y1=cos phi; y2=0.0;
    z0=0.0; z1=0.0; z2=1.0} in
  mat_mul r_z  (mat_mul r_y r_x)

let string_of_vec3 v =
  let open Core in
  Sexp.to_string (Sexp.List [
    Sexp.Atom (string_of_float v.x);
    Sexp.Atom (string_of_float v.y);
    Sexp.Atom (string_of_float v.z)])

let print_vec3 v = print_string (string_of_vec3 v)



