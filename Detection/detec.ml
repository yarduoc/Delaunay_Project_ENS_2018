#use "../common/main.ml";;
#use "matrix.ml"

let is_counterclockwise (p1:point) (p2:point) (p3:point) =  (*Is correct*)
  let det = (p2.x-.p1.x)*.(p3.y-.p1.y) -. (p2.y -. p1.y)*.(p3.x-.p1.x) in
    det >= 0.
;;

let in_circle (t:triangle) (curr_point:point) =
  let in_matrix =  change 1 1 (t.p1.x -. curr_point.x)
                  (change 1 2 (t.p1.y -. curr_point.y)
                  (change 1 3  (   (t.p1.x -. curr_point.x)
                                 *.(t.p1.x -. curr_point.x)
                                 +.(t.p1.y -. curr_point.y)
                                 *.(t.p1.y -. curr_point.y)
                               )
                  (change 2 1 (t.p2.x -. curr_point.x)
                  (change 2 2 (t.p2.y -. curr_point.y)
                  (change 2 3  (   (t.p1.x -. curr_point.x)
                                 *.(t.p1.x -. curr_point.x)
                                 +.(t.p1.y -. curr_point.y)
                                 *.(t.p1.y -. curr_point.y)
                               )
                  (change 3 1 (t.p3.x -. curr_point.x)
                  (change 3 2 (t.p3.y -. curr_point.y)
                  (change 3 3  (   (t.p1.x -. curr_point.x)
                                 *.(t.p1.x -. curr_point.x)
                                 +.(t.p1.y -. curr_point.y)
                                 *.(t.p1.y -. curr_point.y)
                               )
                  (cr_m33() ))))))))) in

  let det = det_3 in_matrix in
  if is_counterclockwise (t.p1) (t.p2) (t.p3)
    then det > 0.
    else det < 0.
  ;;
