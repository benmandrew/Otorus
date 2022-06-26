
open Vec3
open Intersection

let deg_to_rad = Float.pi /. 180.0

let compute_ray x y width height field_of_view =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let fwidth = float_of_int width in
  let fheight = float_of_int height in
  let vx = fx -. (fwidth /. 2.0) in
  let vy = fy -. (fheight /. 2.0) in
  let vz = fheight /. Float.tan((deg_to_rad *. field_of_view) /. 2.0) in
  let v = normalised {x=vx; y=vy; z=vz} in
  {o={x=0.0; y=0.0; z=(-.10.0)}; d=v}

let render_ray ray torus =
  match find_intersection ray torus with
  | None -> None
  | Some _ -> Some (Graphics.rgb 255 0 0)



