module Vec3 : sig
  type t = { x : float; y : float; z : float }

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : float -> t -> t
  val dot : t -> t -> float
  val magnitude2 : t -> float
  val magnitude : t -> float
  val normalised : t -> t
end

module Mat3 : sig
  type t = {
    x0 : float;
    x1 : float;
    x2 : float;
    y0 : float;
    y1 : float;
    y2 : float;
    z0 : float;
    z1 : float;
    z2 : float;
  }

  val ( *** ) : t -> t -> t
  val ( * ) : t -> Vec3.t -> Vec3.t
  val rot_from_euler : float -> float -> float -> t
end

module Ray : sig
  type t = { o : Vec3.t; d : Vec3.t }

  val point_along : t -> float -> Vec3.t
end
