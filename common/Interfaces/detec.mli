type point = { x : float; y : float; }
type triangle = { p0 : point; p1 : point; p3 : point; }
val is_counterclockwise : point -> point -> point -> bool
val in_circle : triangle -> point -> bool
