open Vec3
open Intersection

let deg_to_rad = Float.pi /. 180.0

let compute_ray x y width height field_of_view rot_mat =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let fwidth = float_of_int width in
  let fheight = float_of_int height in
  let vx = fx -. (fwidth /. 2.0) in
  let vy = fy -. (fheight /. 2.0) in
  let vz = fheight /. Float.tan (deg_to_rad *. field_of_view /. 2.0) in
  let d = normalised { x = vx; y = vy; z = vz } in
  let o = { x = 0.0; y = 0.0; z = -15.0 } in
  { o = mat_vec_mul rot_mat o; d = mat_vec_mul rot_mat d }

let compute_normal p t =
  let p_on_circle =
    vec_mul t.maj_r (normalised { x = p.x; y = 0.0; z = p.z })
  in
  normalised (vec_sub p p_on_circle)

let smooth_clamp v =
  { x = v.x /. (v.x +. 0.5); y = v.y /. (v.y +. 0.5); z = v.z /. (v.z +. 0.5) }

let vec3_to_colour v =
  let c = smooth_clamp v in
  Graphics.rgb
    (int_of_float (255.0 *. c.x))
    (int_of_float (255.0 *. c.y))
    (int_of_float (255.0 *. c.z))

let brdf n =
  let value =
    (vec_dot n (normalised { x = -1.0; y = 1.0; z = 1.0 }) /. 2.0) +. 0.5
  in
  let albedo = { x = Float.pow value 2.0; y = 0.0; z = 0.0 } in
  let specular =
    {
      x = Float.pow value 100.0;
      y = Float.pow value 100.0;
      z = Float.pow value 100.0;
    }
  in
  vec3_to_colour (vec_add albedo specular)

let render_ray r t =
  match find_intersection r t with
  | None -> None
  | Some p ->
      let n = compute_normal p t in
      Some (brdf n)
