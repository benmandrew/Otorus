open Torus.Intersection

(* let print_complex (c : Complex.t) =
  print_float c.re;
  print_char ' ';
  print_float c.im;
  print_char 'i';
  print_string " : "

let print_roots (rts : DurandKerner.roots_t) =
  print_complex rts.p;
  print_complex rts.q;
  print_complex rts.r;
  print_complex rts.s;
  print_newline () *)

let () =
  let r : ray_t = {ox=0.0; oy=0.0; oz=(-10.0); dx=0.0; dy=0.0; dz=1.0} in
  let t : torus_t = {maj_r=5.0; min_r=1.0} in
  print_float (find_intersection r t);
  print_newline ()






