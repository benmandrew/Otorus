open Otorus
open Otorus.Linalg

let cam =
  {
    Render.width = 960;
    height = 640;
    pos = Vec.make_point 0.0 0.0 (-100.0);
    field_of_view = 17.0;
    bg_color =
      (let bg_value = 225 in
       Graphics.rgb bg_value bg_value bg_value);
  }

(* Olympic Rings *)
(* let tori =
  let open Transform in
  [
    (* Black ring *)
    Torus.create ~maj_r:5.0 ~min_r:0.6
      {
        x = 0.0;
        y = 2.5;
        z = 0.0;
        psi = -.(Float.pi /. 2.0);
        theta = 0.0;
        phi = -.(Float.pi /. 2.0);
      }
      (Vec.make_vec 0.01 0.01 0.01);
    (* Green ring *)
    Torus.create ~maj_r:5.0 ~min_r:0.6
      {
        x = 6.0;
        y = -2.5;
        z = -0.5;
        psi = -.(Float.pi /. 2.0);
        theta = Float.pi /. 16.0;
        phi = -.(Float.pi /. 2.0);
      }
      (Vec.make_vec 0.0 0.648 0.316);
    (* Yellow ring *)
    Torus.create ~maj_r:5.0 ~min_r:0.6
      {
        x = -6.0;
        y = -2.5;
        z = 0.5;
        psi = -.(Float.pi /. 2.0);
        theta = -.(Float.pi /. 16.0);
        phi = -.(Float.pi /. 2.0);
      }
      (Vec.make_vec 0.984 0.691 0.191);
    (* Red ring *)
    Torus.create ~maj_r:5.0 ~min_r:0.6
      {
        x = 12.0;
        y = 2.5;
        z = 1.5;
        psi = -.(Float.pi /. 2.0);
        theta = 3.0 *. Float.pi /. 16.0;
        phi = -.(Float.pi /. 2.0);
      }
      (Vec.make_vec 0.929 0.199 0.304);
    (* Blue ring *)
    Torus.create ~maj_r:5.0 ~min_r:0.6
      {
        x = -12.0;
        y = 2.5;
        z = -1.5;
        psi = -.(Float.pi /. 2.0);
        theta = -.(3.0 *. Float.pi /. 16.0);
        phi = -.(Float.pi /. 2.0);
      }
      (Vec.make_vec 0.0 0.503 0.781);
  ] *)

(* Interlocked *)
let tori =
   let open Transform in
   [
     Torus.create ~maj_r:5.0 ~min_r:1.0
       {
         x = 3.5;
         y = 0.0;
         z = 0.0;
         psi = -.(Float.pi /. 8.0);
         theta = 0.0;
         phi = -.(Float.pi /. 16.0);
       }
       (Vec.make_vec 1.0 0.5 0.0);
     Torus.create ~maj_r:5.5 ~min_r:0.8
       {
         x = -2.5;
         y = 0.0;
         z = 0.0;
         psi = Float.pi /. 4.0;
         theta = 0.0;
         phi = Float.pi /. 4.0;
       }
       (Vec.make_vec 0.0 0.3 1.0);
   ]
