type point = {x: float; y: float};;
type triangle = {p0 : point; p1 : point; p3 : point};;

let is_counterclockwise (A:point) (B:point) (C:point) =
  let det = (B.x-A.x)*(C.y-A.y) - (B.y - A.y)*(C.x-A.x) in
    det >= 0 
;;

let in_circle (t:triangle) (p:point) = true;;
