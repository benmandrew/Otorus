open Core

module Vec = struct
  type t = { x : float; y : float; z : float; w : float }

  let ( + ) a b =
    let x = a.x +. b.x in
    let y = a.y +. b.y in
    let z = a.z +. b.z in
    let w = a.w +. b.w in
    { x; y; z; w }

  let ( - ) a b =
    let x = a.x -. b.x in
    let y = a.y -. b.y in
    let z = a.z -. b.z in
    let w = a.w -. b.w in
    { x; y; z; w }

  let ( * ) v a =
    let x = v *. a.x in
    let y = v *. a.y in
    let z = v *. a.z in
    let w = v *. a.w in
    { x; y; z; w }

  let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z) +. (a.w *. b.w)
  let magnitude2 v = dot v v
  let magnitude v = Float.sqrt (magnitude2 v)
  let normalised v = 1.0 /. magnitude v * v

  let w_normalised v =
    if Float.equal v.w 0.0 then v
    else { x = v.x /. v.w; y = v.y /. v.w; z = v.z /. v.w; w = 1.0 }

  let make_vec x y z = { x; y; z; w = 0.0 }
  let make_point x y z = { x; y; z; w = 1.0 }
end

module Ray = struct
  type t = { o : Vec.t; d : Vec.t }

  let point_along r d : Vec.t =
    let x = r.o.x +. (r.d.x *. d) in
    let y = r.o.y +. (r.d.y *. d) in
    let z = r.o.z +. (r.d.z *. d) in
    let w = r.o.w +. (r.d.w *. d) in
    { x; y; z; w }
end

module Mat = struct
  type t = {
    x0 : float;
    x1 : float;
    x2 : float;
    x3 : float;
    y0 : float;
    y1 : float;
    y2 : float;
    y3 : float;
    z0 : float;
    z1 : float;
    z2 : float;
    z3 : float;
    w0 : float;
    w1 : float;
    w2 : float;
    w3 : float;
  }

  let ( *** ) b a =
    {
      x0 = (a.x0 *. b.x0) +. (a.y0 *. b.x1) +. (a.z0 *. b.x2) +. (a.w0 *. b.x3);
      x1 = (a.x1 *. b.x0) +. (a.y1 *. b.x1) +. (a.z1 *. b.x2) +. (a.w1 *. b.x3);
      x2 = (a.x2 *. b.x0) +. (a.y2 *. b.x1) +. (a.z2 *. b.x2) +. (a.w2 *. b.x3);
      x3 = (a.x3 *. b.x0) +. (a.y3 *. b.x1) +. (a.z3 *. b.x2) +. (a.w3 *. b.x3);
      y0 = (a.x0 *. b.y0) +. (a.y0 *. b.y1) +. (a.z0 *. b.y2) +. (a.w0 *. b.y3);
      y1 = (a.x1 *. b.y0) +. (a.y1 *. b.y1) +. (a.z1 *. b.y2) +. (a.w1 *. b.y3);
      y2 = (a.x2 *. b.y0) +. (a.y2 *. b.y1) +. (a.z2 *. b.y2) +. (a.w2 *. b.y3);
      y3 = (a.x3 *. b.y0) +. (a.y3 *. b.y1) +. (a.z3 *. b.y2) +. (a.w3 *. b.y3);
      z0 = (a.x0 *. b.z0) +. (a.y0 *. b.z1) +. (a.z0 *. b.z2) +. (a.w0 *. b.z3);
      z1 = (a.x1 *. b.z0) +. (a.y1 *. b.z1) +. (a.z1 *. b.z2) +. (a.w1 *. b.z3);
      z2 = (a.x2 *. b.z0) +. (a.y2 *. b.z1) +. (a.z2 *. b.z2) +. (a.w2 *. b.z3);
      z3 = (a.x3 *. b.z0) +. (a.y3 *. b.z1) +. (a.z3 *. b.z2) +. (a.w3 *. b.z3);
      w0 = (a.x0 *. b.w0) +. (a.y0 *. b.w1) +. (a.z0 *. b.w2) +. (a.w0 *. b.w3);
      w1 = (a.x1 *. b.w0) +. (a.y1 *. b.w1) +. (a.z1 *. b.w2) +. (a.w1 *. b.w3);
      w2 = (a.x2 *. b.w0) +. (a.y2 *. b.w1) +. (a.z2 *. b.w2) +. (a.w2 *. b.w3);
      w3 = (a.x3 *. b.w0) +. (a.y3 *. b.w1) +. (a.z3 *. b.w2) +. (a.w3 *. b.w3);
    }

  let ( * ) a (v : Vec.t) : Vec.t =
    {
      x = (a.x0 *. v.x) +. (a.x1 *. v.y) +. (a.x2 *. v.z) +. (a.x3 *. v.w);
      y = (a.y0 *. v.x) +. (a.y1 *. v.y) +. (a.y2 *. v.z) +. (a.y3 *. v.w);
      z = (a.z0 *. v.x) +. (a.z1 *. v.y) +. (a.z2 *. v.z) +. (a.z3 *. v.w);
      w = (a.w0 *. v.x) +. (a.w1 *. v.y) +. (a.w2 *. v.z) +. (a.w3 *. v.w);
    }

  let transform_ray m r =
    let open Ray in
    { o = Vec.w_normalised (m * r.o); d = m * r.d }

  let rot_from_euler psi theta phi =
    let open Float in
    let r_x =
      {
        x0 = 1.0;
        x1 = 0.0;
        x2 = 0.0;
        x3 = 0.0;
        y0 = 0.0;
        y1 = cos psi;
        y2 = -.sin psi;
        y3 = 0.0;
        z0 = 0.0;
        z1 = sin psi;
        z2 = cos psi;
        z3 = 0.0;
        w0 = 0.0;
        w1 = 0.0;
        w2 = 0.0;
        w3 = 1.0;
      }
    in
    let r_y =
      {
        x0 = cos theta;
        x1 = 0.0;
        x2 = sin theta;
        x3 = 0.0;
        y0 = 0.0;
        y1 = 1.0;
        y2 = 0.0;
        y3 = 0.0;
        z0 = -.sin theta;
        z1 = 0.0;
        z2 = cos theta;
        z3 = 0.0;
        w0 = 0.0;
        w1 = 0.0;
        w2 = 0.0;
        w3 = 1.0;
      }
    in
    let r_z =
      {
        x0 = cos phi;
        x1 = -.sin phi;
        x2 = 0.0;
        x3 = 0.0;
        y0 = sin phi;
        y1 = cos phi;
        y2 = 0.0;
        y3 = 0.0;
        z0 = 0.0;
        z1 = 0.0;
        z2 = 1.0;
        z3 = 0.0;
        w0 = 0.0;
        w1 = 0.0;
        w2 = 0.0;
        w3 = 1.0;
      }
    in
    r_z *** r_y *** r_x

  let translation x y z =
    {
      x0 = 1.0;
      x1 = 0.0;
      x2 = 0.0;
      x3 = x;
      y0 = 0.0;
      y1 = 1.0;
      y2 = 0.0;
      y3 = y;
      z0 = 0.0;
      z1 = 0.0;
      z2 = 1.0;
      z3 = z;
      w0 = 0.0;
      w1 = 0.0;
      w2 = 0.0;
      w3 = 1.0;
    }
end

module Transform = struct
  type t = {
    x : float;
    y : float;
    z : float;
    psi : float;
    theta : float;
    phi : float;
  }

  let generate tr =
    let open Mat in
    let rotation = rot_from_euler tr.psi tr.theta tr.phi in
    let translation = translation tr.x tr.y tr.z in
    translation *** rotation

  let generate_inverse tr =
    let open Mat in
    let rotation = rot_from_euler (-.tr.psi) (-.tr.theta) (-.tr.phi) in
    let translation = translation (-.tr.x) (-.tr.y) (-.tr.z) in
    rotation *** translation
end
