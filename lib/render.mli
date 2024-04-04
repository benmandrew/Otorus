open Linalg

type camera = {
  width : int;
  height : int;
  pos : Linalg.Vec.t;
  field_of_view : float;
  bg_color : int * int * int;
}

val compute_ray : cam:camera -> int -> int -> Ray.t
val render_ray : Ray.t -> Torus.t list -> (int * int * int) option
