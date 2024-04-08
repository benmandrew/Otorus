type color = int * int * int * int

val background : color
val r : color -> int
val g : color -> int
val b : color -> int
val a : color -> int

type image = color array array
