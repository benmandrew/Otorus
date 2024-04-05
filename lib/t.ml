(* RGBA *)
type colour = int * int * int * int

let background = (225, 225, 225, 0)
let r (r, _, _, _) = r
let g (_, g, _, _) = g
let b (_, _, b, _) = b
let a (_, _, _, a) = a

type image = colour array array
