open Linalg

type camera = {
  width : int;
  height : int;
  pos : Linalg.Vec.t;
  field_of_view : float;
  bg_colour : T.colour;
}

let deg_to_rad = Float.pi /. 180.0

let compute_ray ~cam x y : Ray.t =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let fwidth = float_of_int cam.width in
  let fheight = float_of_int cam.height in
  let vx = fx -. (fwidth /. 2.0) in
  let vy = fy -. (fheight /. 2.0) in
  let vz = fheight /. Float.tan (deg_to_rad *. cam.field_of_view /. 2.0) in
  let d = Vec.normalised (Vec.make_vec vx vy vz) in
  { o = cam.pos; d }

let smooth_clamp (v : Vec.t) : Vec.t =
  let f x = x /. (x +. 0.1) in
  Vec.make_vec (f v.x) (f v.y) (f v.z)

let vec_to_color v =
  let c = smooth_clamp v in
  ( int_of_float (255.0 *. c.x),
    int_of_float (255.0 *. c.y),
    int_of_float (255.0 *. c.z) )

let light_pos = Vec.make_point 50.0 10.0 (-30.0)

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

let brdf t p n =
  let open Vec in
  let ambient = 0.01 * Torus.color t in
  let value =
    let unclamped = dot n (normalised (light_pos - p)) in
    Float.max ((unclamped +. 0.5) /. 1.5) 0.0
  in
  let albedo = Float.pow value 2.0 * Torus.color t in
  let spec_val = Float.pow value 100.0 in
  let specular = make_vec spec_val spec_val spec_val in
  vec_to_color (ambient + albedo + specular)

let render_ray r ts =
  match intersect_ray r ts with
  | None -> None
  | Some (t, p) ->
      let n = Torus.normal t p in
      Some (brdf t p n)
