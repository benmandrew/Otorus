let n_tiles_x = ref 12
let n_tiles_y = ref 8

let indices ?(random = false) width height =
  let n = width * height in
  let a = Array.init n (fun i -> (i mod width, height - (i / width) - 1)) in
  if random then
    for i = n - 1 downto 1 do
      let k = Random.int (i + 1) in
      let x = a.(k) in
      a.(k) <- a.(i);
      a.(i) <- x
    done;
  a

module type S = sig
  val set_render_dims_xy : x:int -> y:int -> unit
  val set_render_dims_y : y:int -> unit

  val render :
    cam:Render.camera -> draw:(T.image -> unit) -> Torus.t list -> unit
end

module Make (S : sig
  val render :
    cam:Render.camera -> draw:(T.image -> unit) -> Torus.t list -> unit
end) =
struct
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
      | None -> img.(cam.height - y - 1).(x) <- T.background
      | Some c -> img.(cam.height - y - 1).(x) <- c
    done
  done

module Dt = Domainslib.Task

let n_cores = 4

module ParallelTile = Make (struct
  let render ~cam ~draw tori =
    let tile_width = cam.Render.width / !n_tiles_x in
    let tile_height = cam.height / !n_tiles_y in
    let img = Array.make_matrix cam.height cam.width T.background in
    let indices = indices !n_tiles_x !n_tiles_y in
    let pool = Dt.setup_pool ~num_domains:(n_cores - 1) () in
    let f (x, y) =
      let x0, y0, x1, y1 =
        ( tile_width * x,
          tile_height * y,
          tile_width * (x + 1),
          tile_height * (y + 1) )
      in
      Dt.async pool (fun () -> render_tile ~cam tori img x0 y0 x1 y1)
    in
    Dt.run pool (fun () -> Array.map f indices |> Array.iter (Dt.await pool));
    Dt.teardown_pool pool;
    draw img
end)

module ParallelRowAuto = Make (struct
  let render ~cam ~draw tori =
    let img = Array.make_matrix cam.Render.height cam.width T.background in
    let pool = Dt.setup_pool ~num_domains:(n_cores - 1) () in
    let f i =
      let x, y = (i mod cam.width, i / cam.width) in
      match render_pixel ~cam tori x y with
      | None -> ()
      | Some c -> img.(cam.height - y - 1).(x) <- c
    in
    Dt.run pool (fun () ->
        Dt.parallel_for ~start:0 ~finish:(cam.height * cam.width) ~body:f pool);
    Dt.teardown_pool pool;
    draw img
end)

module ParallelColumnAuto = Make (struct
  let render ~cam ~draw tori =
    let img = Array.make_matrix cam.Render.height cam.width T.background in
    let pool = Dt.setup_pool ~num_domains:(n_cores - 1) () in
    let f i =
      let x, y = (i / cam.height, i mod cam.height) in
      match render_pixel ~cam tori x y with
      | None -> ()
      | Some c -> img.(cam.height - y - 1).(x) <- c
    in
    Dt.run pool (fun () ->
        Dt.parallel_for ~start:0 ~finish:(cam.height * cam.width) ~body:f pool);
    Dt.teardown_pool pool;
    draw img
end)

module SequentialTile = Make (struct
  let render ~cam ~draw tori =
    let tile_width = cam.Render.width / !n_tiles_x in
    let tile_height = cam.height / !n_tiles_y in
    let img = Array.make_matrix cam.height cam.width T.background in
    let indices = indices !n_tiles_x !n_tiles_y in
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
end)

open Cmdliner

let cmdliner =
  let parallel_tile =
    Arg.info ~doc:"Render by tiles in parallel." ~docv:"PARALLEL_TILE"
      [ "parallel-tile" ]
  in
  let parallel_row_auto =
    Arg.info ~doc:"Render by rows in parallel with automatic work scheduling."
      ~docv:"PARALLEL_ROW_AUTO" [ "parallel-row-auto" ]
  in
  let parallel_column_auto =
    Arg.info
      ~doc:"Render by columns in parallel with automatic work scheduling."
      ~docv:"PARALLEL_COLUMN_AUTO" [ "parallel-column-auto" ]
  in
  let sequential_tile =
    Arg.info ~doc:"Render by tiles sequentially." ~docv:"SEQUENTIAL_TILE"
      [ "sequential-tile" ]
  in
  Arg.value
  @@ Arg.vflag
       (module ParallelRowAuto : S)
       [
         ((module ParallelTile), parallel_tile);
         ((module ParallelRowAuto), parallel_row_auto);
         ((module ParallelColumnAuto), parallel_column_auto);
         ((module SequentialTile), sequential_tile);
       ]
