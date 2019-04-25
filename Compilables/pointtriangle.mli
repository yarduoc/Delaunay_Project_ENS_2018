(*© Copyright Paul Bastide, Alex Coudray, Lauric Desauw 25/04/2019 CC-BY 4.0*)

type point
type triangle
type morph_point
type point_set
type triangle_set
type morph_point_set
val make_point : float -> float -> point
val get_x : point -> float
val get_y : point -> float
val make_triangle : point -> point -> point -> triangle
val get_first_point : triangle -> point
val get_second_point : triangle -> point
val get_third_point : triangle -> point
val make_morph_point : float -> float -> int -> morph_point
val point_to_morph_point : point -> morph_point
val morph_point_to_point : morph_point -> point
val print_point : point -> unit
val print_triangle : triangle -> unit
val print_triangle_set : triangle_set -> unit
val ord_points : point -> point -> bool
val ord_m_points : morph_point -> morph_point -> bool
