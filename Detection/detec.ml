type point = {x: float; y: float};;
type triangle = {p0 : point; p1 : point; p3 : point};;

let is_counterclockwise (p0:point) (p1:point) (p2:point) = true;;

let in_circle (t:triangle) (p:point) = true;;
