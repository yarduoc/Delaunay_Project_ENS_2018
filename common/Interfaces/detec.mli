type point = { x : float; y : float; }
type triangle = { p1 : point; p2 : point; p3 : point; }
type point_set = point list
type triangle_set = triangle list


val is_counterclockwise : point -> point -> point -> bool
val in_circle : triangle -> point -> bool
