open Linalg

let deg_to_rad = Float.pi /. 180.0

let compute_ray x y width height field_of_view rot_mat : Ray.t =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let fwidth = float_of_int width in
  let fheight = float_of_int height in
  let vx = fx -. (fwidth /. 2.0) in
  let vy = fy -. (fheight /. 2.0) in
  let vz = fheight /. Float.tan (deg_to_rad *. field_of_view /. 2.0) in
  let d = Vec3.normalised { x = vx; y = vy; z = vz } in
  let o : Vec3.t = { x = 0.0; y = 0.0; z = -15.0 } in
  let open Mat3 in
  { o = rot_mat * o; d = rot_mat * d }

let smooth_clamp (v : Vec3.t) : Vec3.t =
  { x = v.x /. (v.x +. 0.5); y = v.y /. (v.y +. 0.5); z = v.z /. (v.z +. 0.5) }

let vec3_to_colour v : Graphics.color =
  let c = smooth_clamp v in
  Graphics.rgb
    (int_of_float (255.0 *. c.x))
    (int_of_float (255.0 *. c.y))
    (int_of_float (255.0 *. c.z))

let brdf n : Graphics.color =
  let open Vec3 in
  let value =
    (dot n (normalised { x = -1.0; y = 1.0; z = 1.0 }) /. 2.0) +. 0.5
  in
  let albedo = { x = Float.pow value 2.0; y = 0.0; z = 0.0 } in
  let specular =
    {
      x = Float.pow value 100.0;
      y = Float.pow value 100.0;
      z = Float.pow value 100.0;
    }
  in
  vec3_to_colour (albedo + specular)

let render_ray r t : Graphics.color option =
  match Torus.intersection t r with
  | None -> None
  | Some p ->
      let n = Torus.normal t p in
      Some (brdf n)
