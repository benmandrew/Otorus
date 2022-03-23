open Quartic

let epsilon = 0.001

type ray_t = {ox : float; oy : float; oz : float; dx : float; dy : float; dz : float}

type torus_t = {maj_r : float; min_r : float}

(* http://blog.marcinchwedczuk.pl/ray-tracing-torus *)
let generate_quartic r t =
  let a_sqrt = r.dx *. r.dx +. r.dy *. r.dy +. r.dz *. r.dz in
  let od = r.ox *. r.dx +. r.oy *. r.dy +. r.oz *. r.dz in
  let orr = r.ox *. r.ox +. r.oy *. r.oy +. r.oz *. r.oz -. (t.min_r *. t.min_r +. t.maj_r *. t.maj_r) in
  let a = a_sqrt *. a_sqrt in
  let b = 4. *. a_sqrt *. od in
  let c = 2. *. a_sqrt *. orr +. 4. *. od *. od +. 4. *. t.maj_r *. t.maj_r *. r.dy *. r.dy in
  let d = 4. *. orr *. od +. 8. *. t.maj_r *. t.maj_r *. r.oy *. r.dy in
  let e = orr *. orr -. 4. *. t.maj_r *. t.maj_r *. (t.min_r *. t.min_r -. r.oy *. r.oy) in
  let open Quartic in
  {a; b; c; d; e}

let find_intersection r t =
  let q = generate_quartic r t in
  let roots = DurandKerner.get_real_roots q in
  let dist = List.filter (fun x -> x > epsilon) roots |> List.fold_left min infinity in
  dist











(* let ray_torus_intersection (r : ray_t) (t : torus_t) =
  true *)



