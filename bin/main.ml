open Otorus
open Otorus.Linalg

let width = 960
let height = 640
let pos = Vec.make_point 0.0 0.0 (-25.0)
let field_of_view = 60.0
let n_tiles_x = 12
let n_tiles_y = 8
let tile_width = width / n_tiles_x
let tile_height = height / n_tiles_y

let tori =
  let open Transform in
  [
    Torus.create 5.0 1.0
      {
        x = 3.5;
        y = 0.0;
        z = 0.0;
        psi = -.(Float.pi /. 8.0);
        theta = 0.0;
        phi = -.(Float.pi /. 16.0);
      }
      (Vec.make_vec 1.0 0.0 0.0);
    Torus.create 5.5 0.8
      {
        x = -2.5;
        y = 0.0;
        z = 0.0;
        psi = Float.pi /. 4.0;
        theta = 0.0;
        phi = Float.pi /. 4.0;
      }
      (Vec.make_vec 0.0 1.0 0.0);
  ]

let bg_value = 225
let bg_colour = Graphics.rgb bg_value bg_value bg_value

let exit_handler (status : Graphics.status) =
  if status.keypressed && status.key == ' ' then raise Exit else ()

let init_window () =
  let open Graphics in
  open_graph "";
  resize_window width height;
  set_color bg_colour;
  fill_rect 0 0 width height

let render_pixel x y =
  let ray = Render.compute_ray x y width height pos field_of_view in
  match Render.render_ray ray tori with None -> bg_colour | Some c -> c

let render_tile x0 y0 x1 y1 =
  Array.init (y1 - y0) (fun y ->
      Array.init (x1 - x0) (fun x -> render_pixel (x0 + x) (y1 - y)))
(* |> Graphics.make_image *)

module T = Domainslib.Task

let () =
  let start = Unix.gettimeofday () in
  init_window ();
  let pool = T.setup_pool ~num_domains:3 () in
  let results =
    T.run pool (fun () ->
        let jobs =
          Array.init n_tiles_y (fun y ->
              Array.init n_tiles_x (fun x ->
                  let x0, y0, x1, y1 =
                    ( tile_width * x,
                      tile_height * y,
                      tile_width * (x + 1),
                      tile_height * (y + 1) )
                  in
                  T.async pool (fun () -> render_tile x0 y0 x1 y1)))
        in
        Array.map (fun row -> Array.map (fun j -> T.await pool j) row) jobs)
  in
  for x = 0 to n_tiles_x - 1 do
    for y = 0 to n_tiles_y - 1 do
      let x0, y0 = (tile_width * x, tile_height * y) in
      let img = Graphics.make_image results.(y).(x) in
      Graphics.draw_image img x0 y0
    done
  done;
  T.teardown_pool pool;
  let stop = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs\n%!" (stop -. start);
  Graphics.loop_at_exit [ Key_pressed ] exit_handler
