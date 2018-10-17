#use "../common/main.ml";;

let is_counterclockwise (p1:point) (p2:point) (p3:point) =
  let det = (p2.x-.p1.x)*.(p3.y-.p1.y) -. (p2.y -. p1.y)*.(p3.x-.p1.x) in
    det >= 0.
;;

let in_circle (t:triangle) (curr_point:point) =
  let det1 = (t.p1.x)*.(t.p2.y) -. (t.p2.x)*.(t.p1.y) in
  let det2 = ((t.p1.x)*.(t.p1.x) +. (t.p1.y)*.(t.p1.y)) -. ((t.p2.x)*.(t.p2.x) +. (t.p2.y)*.(t.p2.y)) in
  let det3 = (t.p3.x)*.(curr_point.y) -. (curr_point.x)*.(t.p3.y) in
  let det4 = ((t.p3.x)*.(t.p3.x) +. (t.p3.y)*.(t.p3.y)) -. ((curr_point.x)*.(curr_point.x) +. (curr_point.y)*.(curr_point.y)) in
    if is_counterclockwise (t.p1) (t.p2) (t.p3)
    then (det1*.det4 -. det2*.det3) > 0.
    else (det1*.det4 -. det2*.det3) < 0. ;;
