open Linalg

let deg_to_rad = Float.pi /. 180.0

let compute_ray x y width height pos field_of_view : Ray.t =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let fwidth = float_of_int width in
  let fheight = float_of_int height in
  let vx = fx -. (fwidth /. 2.0) in
  let vy = fy -. (fheight /. 2.0) in
  let vz = fheight /. Float.tan (deg_to_rad *. field_of_view /. 2.0) in
  let d = Vec.normalised (Vec.make_vec vx vy vz) in
  { o = pos; d }

let smooth_clamp (v : Vec.t) : Vec.t =
  let f x = x /. (x +. 0.1) in
  Vec.make_vec (f v.x) (f v.y) (f v.z)

let vec_to_colour v : Graphics.color =
  let c = smooth_clamp v in
  Graphics.rgb
    (int_of_float (255.0 *. c.x))
    (int_of_float (255.0 *. c.y))
    (int_of_float (255.0 *. c.z))

let light_pos = Vec.make_point 50.0 10.0 (-30.0)

(* let light_pos = Vec.make_point (-100.0) 0.0 0.0 *)

let intersect_ray r ts =
  let intersections =
    List.filter_map
      (fun t ->
        match Torus.intersection t r with
        | None -> None
        | Some (p, d) -> Some (t, p, d))
      ts
  in
  match intersections with
  | [] -> None
  | head :: tail ->
      let t, p, _ =
        List.fold_left
          (fun (t0, p0, d0) (t1, p1, d1) ->
            if Float.compare d0 d1 < 0 then (t0, p0, d0) else (t1, p1, d1))
          head tail
      in
      Some (t, p)

let brdf t p n : Graphics.color =
  let open Vec in
  let ambient = 0.01 * Torus.colour t in
  let value = Float.max (dot n (normalised (light_pos - p))) 0.0 in
  let albedo = Float.pow value 2.0 * Torus.colour t in
  let spec_val = Float.pow value 100.0 in
  let specular = make_vec spec_val spec_val spec_val in
  vec_to_colour (ambient + albedo + specular)

let render_ray r ts : Graphics.color option =
  match intersect_ray r ts with
  | None -> None
  | Some (t, p) ->
      let n = Torus.normal t p in
      Some (brdf t p n)
