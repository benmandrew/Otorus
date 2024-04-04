type image = (int * int * int) array array

let n_tiles_x = ref 12
let n_tiles_y = ref 8

let random_indices width height =
  let n = width * height in
  let a = Array.init n (fun i -> (i mod width, height - (i / width) - 1)) in
  (* for i = n - 1 downto 1 do
       let k = Random.int (i + 1) in
       let x = a.(k) in
       a.(k) <- a.(i);
       a.(i) <- x
     done; *)
  a

module type ENGINE = sig
  type nonrec image = image

  val set_render_dims_xy : x:int -> y:int -> unit
  val set_render_dims_y : y:int -> unit
  val render : draw:(image -> unit) -> cam:Render.camera -> Torus.t list -> unit
end

module Make (S : sig
  val render : draw:(image -> unit) -> cam:Render.camera -> Torus.t list -> unit
end) =
struct
  type nonrec image = image

  let set_render_dims_xy ~x ~y =
    n_tiles_x := x;
    n_tiles_y := y

  let set_render_dims_y ~y = n_tiles_y := y
  let render = S.render
end

let render_pixel ~cam tori x y =
  let ray = Render.compute_ray ~cam x y in
  Render.render_ray ray tori

let render_tile ~cam tori img x0 y0 x1 y1 =
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      match render_pixel ~cam tori x y with
      | None -> img.(cam.height - y - 1).(x) <- cam.bg_color
      | Some c -> img.(cam.height - y - 1).(x) <- c
    done
  done

module T = Domainslib.Task

let n_cores = 4

module Pt = struct
  let render ~draw ~(cam : Render.camera) tori =
    let tile_width = cam.width / !n_tiles_x in
    let tile_height = cam.height / !n_tiles_y in
    let img = Array.make_matrix cam.height cam.width cam.bg_color in
    let indices = random_indices !n_tiles_x !n_tiles_y in
    let pool = T.setup_pool ~num_domains:(n_cores - 1) () in
    let f (x, y) =
      let x0, y0, x1, y1 =
        ( tile_width * x,
          tile_height * y,
          tile_width * (x + 1),
          tile_height * (y + 1) )
      in
      T.async pool (fun () -> render_tile ~cam tori img x0 y0 x1 y1)
    in
    T.run pool (fun () -> Array.map f indices |> Array.iter (T.await pool));
    T.teardown_pool pool;
    draw img
end

module Pra = struct
  let render ~draw ~(cam : Render.camera) tori =
    let img = Array.make_matrix cam.height cam.width cam.bg_color in
    let pool = T.setup_pool ~num_domains:(n_cores - 1) () in
    let f i =
      let x, y = (i mod cam.width, i / cam.width) in
      match render_pixel ~cam tori x y with
      | None -> ()
      | Some c -> img.(cam.height - y - 1).(x) <- c
    in
    T.run pool (fun () ->
        T.parallel_for ~start:0 ~finish:(cam.height * cam.width) ~body:f pool);
    T.teardown_pool pool;
    draw img
end

module Pca = struct
  let render ~draw ~(cam : Render.camera) tori =
    let img = Array.make_matrix cam.height cam.width cam.bg_color in
    let pool = T.setup_pool ~num_domains:(n_cores - 1) () in
    let f i =
      let x, y = (i / cam.height, i mod cam.height) in
      match render_pixel ~cam tori x y with
      | None -> ()
      | Some c -> img.(cam.height - y - 1).(x) <- c
    in
    T.run pool (fun () ->
        T.parallel_for ~start:0 ~finish:(cam.height * cam.width) ~body:f pool);
    T.teardown_pool pool;
    draw img
end

module St = struct
  let render ~draw ~(cam : Render.camera) tori =
    let tile_width = cam.width / !n_tiles_x in
    let tile_height = cam.height / !n_tiles_y in
    let img = Array.make_matrix cam.height cam.width (0, 0, 0) in
    let indices = random_indices !n_tiles_x !n_tiles_y in
    let f (x, y) =
      let x0, y0, x1, y1 =
        ( tile_width * x,
          tile_height * y,
          tile_width * (x + 1),
          tile_height * (y + 1) )
      in
      render_tile ~cam tori img x0 y0 x1 y1;
      draw img
    in
    Array.iter f indices
end

module ParallelTile = Make (Pt)
module ParallelRowAuto = Make (Pra)
module ParallelColumnAuto = Make (Pca)
module SequentialTile = Make (St)
