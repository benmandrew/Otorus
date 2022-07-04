module Vec : sig
  type t = { x : float; y : float; z : float; w : float }

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : float -> t -> t
  val dot : t -> t -> float
  val magnitude2 : t -> float
  val magnitude : t -> float
  val normalised : t -> t
  val w_normalised : t -> t
  val make_vec : float -> float -> float -> t
  val make_point : float -> float -> float -> t
end

module Ray : sig
  type t = { o : Vec.t; d : Vec.t }

  val point_along : t -> float -> Vec.t
end

module Mat : sig
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

  val ( *** ) : t -> t -> t
  val ( * ) : t -> Vec.t -> Vec.t
  val transform_ray : t -> Ray.t -> Ray.t
  val rot_from_euler : float -> float -> float -> t
  val translation : float -> float -> float -> t
end

module Transform : sig
  type t = {
    x : float;
    y : float;
    z : float;
    psi : float;
    theta : float;
    phi : float;
  }

  val generate : t -> Mat.t
  val generate_inverse : t -> Mat.t
end
