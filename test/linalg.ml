open Core
open Otorus.Linalg

(* Vec tests *)

let%test_unit "vec_constructor" =
  let v = Vec.make_vec 3.0 1.0 2.0 in
  [%test_eq: float] v.x 3.0;
  [%test_eq: float] v.y 1.0;
  [%test_eq: float] v.z 2.0;
  [%test_eq: float] v.w 0.0

let%test_unit "point_constructor" =
  let v = Vec.make_point 5.0 2.0 0.0 in
  [%test_eq: float] v.x 5.0;
  [%test_eq: float] v.y 2.0;
  [%test_eq: float] v.z 0.0;
  [%test_eq: float] v.w 1.0

let%test_unit "vec_add" =
  let v0 = Vec.make_vec 3.0 1.0 2.0 in
  let v1 = Vec.make_vec 10.0 20.0 9.0 in
  let res = Vec.( + ) v0 v1 in
  [%test_eq: float] res.x 13.0;
  [%test_eq: float] res.y 21.0;
  [%test_eq: float] res.z 11.0;
  [%test_eq: float] res.w 0.0

let%test_unit "vec_point_add" =
  let v0 = Vec.make_vec 3.0 1.0 2.0 in
  let v1 = Vec.make_point 10.0 20.0 9.0 in
  let res = Vec.( + ) v0 v1 in
  [%test_eq: float] res.x 13.0;
  [%test_eq: float] res.y 21.0;
  [%test_eq: float] res.z 11.0;
  [%test_eq: float] res.w 1.0

let%test_unit "vec_point_sub" =
  let v0 = Vec.make_point 10.0 20.0 9.0 in
  let v1 = Vec.make_vec 3.0 1.0 2.0 in
  let res = Vec.( - ) v0 v1 in
  [%test_eq: float] res.x 7.0;
  [%test_eq: float] res.y 19.0;
  [%test_eq: float] res.z 7.0;
  [%test_eq: float] res.w 1.0

let%test_unit "vec_mul" =
  let v = Vec.make_vec 7.0 8.0 9.0 in
  let res = Vec.( * ) 5.0 v in
  [%test_eq: float] res.x 35.0;
  [%test_eq: float] res.y 40.0;
  [%test_eq: float] res.z 45.0;
  [%test_eq: float] res.w 0.0

let%test_unit "vec_w_normalised" =
  let v = Vec.make_vec 7.0 8.0 9.0 in
  let res = Vec.w_normalised v in
  [%test_eq: float] res.x 7.0;
  [%test_eq: float] res.y 8.0;
  [%test_eq: float] res.z 9.0;
  [%test_eq: float] res.w 0.0

let%test_unit "point_w_normalised" =
  let v0 = Vec.make_point 6.0 8.0 4.0 in
  let v1 = Vec.make_point 0.0 0.0 0.0 in
  let res = Vec.(w_normalised (v0 + v1)) in
  [%test_eq: float] res.x 3.0;
  [%test_eq: float] res.y 4.0;
  [%test_eq: float] res.z 2.0;
  [%test_eq: float] res.w 1.0

(* Ray tests *)

let%test_unit "ray_create" =
  let o = Vec.make_point 6.0 8.0 4.0 in
  let d = Vec.make_vec 0.0 1.0 0.0 in
  let res = Ray.create o d in
  [%test_eq: float] res.o.x 6.0;
  [%test_eq: float] res.o.y 8.0;
  [%test_eq: float] res.o.z 4.0;
  [%test_eq: float] res.o.w 1.0;
  [%test_eq: float] res.d.x 0.0;
  [%test_eq: float] res.d.y 1.0;
  [%test_eq: float] res.d.z 0.0;
  [%test_eq: float] res.d.w 0.0

let%test_unit "ray_point_along" =
  let o = Vec.make_point 6.0 8.0 4.0 in
  let d = Vec.make_vec 0.0 1.0 0.0 in
  let r = Ray.create o d in
  let res = Ray.point_along r 5.0 in
  [%test_eq: float] res.x 6.0;
  [%test_eq: float] res.y 13.0;
  [%test_eq: float] res.z 4.0;
  [%test_eq: float] res.w 1.0
