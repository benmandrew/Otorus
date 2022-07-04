open Linalg

let deg_to_rad = Float.pi /. 180.0

let compute_ray x y width height field_of_view : Ray.t =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let fwidth = float_of_int width in
  let fheight = float_of_int height in
  let vx = fx -. (fwidth /. 2.0) in
  let vy = fy -. (fheight /. 2.0) in
  let vz = fheight /. Float.tan (deg_to_rad *. field_of_view /. 2.0) in
  let d = Vec.normalised (Vec.make_vec vx vy vz) in
  let o = Vec.make_point 0.0 0.0 (-15.0) in
  { o; d }

let smooth_clamp (v : Vec.t) : Vec.t =
  let f x = x /. (x +. 0.5) in
  Vec.make_vec (f v.x) (f v.y) (f v.z)

let vec_to_colour v : Graphics.color =
  let c = smooth_clamp v in
  Graphics.rgb
    (int_of_float (255.0 *. c.x))
    (int_of_float (255.0 *. c.y))
    (int_of_float (255.0 *. c.z))

let brdf n : Graphics.color =
  let open Vec in
  let value = (dot n (normalised (make_vec (-1.0) 1.0 1.0)) /. 2.0) +. 0.5 in
  let albedo = make_vec (Float.pow value 2.0) 0.0 0.0 in
  let specular =
    make_vec (Float.pow value 100.0) (Float.pow value 100.0)
      (Float.pow value 100.0)
  in
  vec_to_colour (albedo + specular)

let render_ray r t : Graphics.color option =
  match Torus.intersection t r with
  | None -> None
  | Some p ->
      let n = Torus.normal t p in
      Some (brdf n)
