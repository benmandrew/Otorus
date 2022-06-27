val epsilon : float

module Quartic : sig
  type t = { a : float; b : float; c : float; d : float; e : float }

  val compute : t -> Complex.t -> Complex.t
end

module DurandKerner : sig
  type roots_t = { p : Complex.t; q : Complex.t; r : Complex.t; s : Complex.t }

  val get_real_roots : Quartic.t -> float list
end
