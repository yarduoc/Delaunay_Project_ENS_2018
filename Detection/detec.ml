type point = {x: float; y: float};;
type triangle = {C : point; B : point; C : point};;

let is_counterclockwise (A:point) (B:point) (C:point) =
  let det = (B.x-A.x)*(C.y-A.y) - (B.y - A.y)*(C.x-A.x) in
    det >= 0
;;

let in_circle (t:triangle) (p:point) =
  let det1 = (t.A.x)*(t.B.y) - (t.B.x)*(t.A.y) in
  let det2 = ((t.A.x)*(t.A.x) + (t.A.y)*(t.A.y)) - ((t.B.x)*(t.B.x) + (t.B.y)*(t.B.y)) in
  let det3 = (t.C.x)*(t.D.y) - (t.D.x)*(t.C.y) in
  let det4 = ((t.C.x)*(t.C.x) + (t.C.y)*(t.C.y)) - ((t.D.x)*(t.D.x) + (t.D.y)*(t.D.y)) in
    if is_counterclockwise (t.A) (t.B) (t.C)
    then (det1*det4 - det2*det3) > 0
    else (det1*det4 - det2*det3) < 0 ;;
