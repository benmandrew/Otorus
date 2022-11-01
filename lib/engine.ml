let n_tiles_x = ref 12
let n_tiles_y = ref 8

(* let n_tiles_y = 10 *)

let set_render_dims_xy ~x ~y =
  n_tiles_x := x;
  n_tiles_y := y

let set_render_dims_y ~y = n_tiles_y := y

let render_pixel ~cam tori x y =
  let ray = Render.compute_ray ~cam x y in
  Render.render_ray ray tori

let render_tile ~cam tori img x0 y0 x1 y1 =
  for x = x0 to x1 - 1 do
    for y = y0 to y1 - 1 do
      match render_pixel ~cam tori x y with
      | None -> ()
      | Some c -> img.(cam.height - y - 1).(x) <- c
    done
  done

let random_indices width height =
  let n = width * height in
  let a = Array.init n (fun i -> (i mod width, i / width)) in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

module type ENGINE = sig
  val render : cam:Render.camera -> Torus.t list -> unit
end

module ParallelTile : ENGINE = struct
  let n_cores = 4

  module T = Domainslib.Task

  let render ~(cam : Render.camera) tori =
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
    Graphics.draw_image (Graphics.make_image img) 0 0
end

module ParallelRow : ENGINE = struct
  let n_cores = 4

  module T = Domainslib.Task

  let render ~(cam : Render.camera) tori =
    let row_height = cam.height / !n_tiles_y in
    let img = Array.make_matrix cam.height cam.width cam.bg_color in
    let indices = random_indices 1 !n_tiles_y in
    let pool = T.setup_pool ~num_domains:(n_cores - 1) () in
    let f (_, y) =
      let x0, y0, x1, y1 =
        (0, row_height * y, cam.width, row_height * (y + 1))
      in
      T.async pool (fun () -> render_tile ~cam tori img x0 y0 x1 y1)
    in
    T.run pool (fun () -> Array.map f indices |> Array.iter (T.await pool));
    T.teardown_pool pool;
    Graphics.draw_image (Graphics.make_image img) 0 0
end

module SequentialTile : ENGINE = struct
  let render ~(cam : Render.camera) tori =
    let tile_width = cam.width / !n_tiles_x in
    let tile_height = cam.height / !n_tiles_y in
    let img = Array.make_matrix cam.height cam.width cam.bg_color in
    let indices = random_indices !n_tiles_x !n_tiles_y in
    let f (x, y) =
      let x0, y0, x1, y1 =
        ( tile_width * x,
          tile_height * y,
          tile_width * (x + 1),
          tile_height * (y + 1) )
      in
      render_tile ~cam tori img x0 y0 x1 y1;
      Graphics.draw_image (Graphics.make_image img) 0 0
    in
    Array.iter f indices
end

module SequentialRow : ENGINE = struct
  let render ~(cam : Render.camera) tori =
    let row_height = cam.height / !n_tiles_y in
    let img = Array.make_matrix cam.height cam.width cam.bg_color in
    let indices = random_indices 1 !n_tiles_y in
    let f (_, y) =
      let x0, y0, x1, y1 =
        (0, row_height * y, cam.width, row_height * (y + 1))
      in
      render_tile ~cam tori img x0 y0 x1 y1;
      Graphics.draw_image (Graphics.make_image img) 0 0
    in
    Array.iter f indices
end
