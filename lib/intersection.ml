open Vec3
open Quartic

let epsilon = 0.001

type ray_t = {o : vec3_t; d : vec3_t}

type torus_t = {maj_r : float; min_r : float}

(* http://blog.marcinchwedczuk.pl/ray-tracing-torus *)
let generate_quartic r t =
  let a_sqrt = r.d.x *. r.d.x +. r.d.y *. r.d.y +. r.d.z *. r.d.z in
  let od = r.o.x *. r.d.x +. r.o.y *. r.d.y +. r.o.z *. r.d.z in
  let orr = r.o.x *. r.o.x +. r.o.y *. r.o.y +. r.o.z *. r.o.z -. (t.min_r *. t.min_r +. t.maj_r *. t.maj_r) in
  let a = a_sqrt *. a_sqrt in
  let b = 4. *. a_sqrt *. od in
  let c = 2. *. a_sqrt *. orr +. 4. *. od *. od +. 4. *. t.maj_r *. t.maj_r *. r.d.y *. r.d.y in
  let d = 4. *. orr *. od +. 8. *. t.maj_r *. t.maj_r *. r.o.y *. r.d.y in
  let e = orr *. orr -. 4. *. t.maj_r *. t.maj_r *. (t.min_r *. t.min_r -. r.o.y *. r.o.y) in
  let open Quartic in
  {a; b; c; d; e}

let find_intersection r t =
  let q = generate_quartic r t in
  let roots = DurandKerner.get_real_roots q in
  let dist = List.filter (fun x -> x > epsilon) roots |> List.fold_left min infinity in
  dist

let point_along_ray r d =
  let x = r.o.x +. r.d.x *. d in
  let y = r.o.y +. r.d.y *. d in
  let z = r.o.z +. r.d.z *. d in
  {x; y; z}









(* let ray_torus_intersection (r : ray_t) (t : torus_t) =
  true *)



