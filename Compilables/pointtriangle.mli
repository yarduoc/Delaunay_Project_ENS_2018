type point
type triangle
type point_set
type triangle_set 
val make_point : float -> float -> point
val get_x : point -> float
val get_y : point -> float
val make_triangle : point -> point -> point -> triangle
val get_first_point : triangle -> point
val get_second_point : triangle -> point
val get_third_point : triangle -> point
