#use "../common/alphaset.ml";;
#use "../common/main.ml";;
#use "matrix.ml"

let is_counterclockwise (p1:point) (p2:point) (p3:point) =  (*Is correct*)
  let det = (p2.x-.p1.x)*.(p3.y-.p1.y) -. (p2.y -. p1.y)*.(p3.x-.p1.x) in
    det >= 0.
;;

let in_circle (t:triangle) (curr_point:point) =
  let in_matrix =  [| [| (t.p1.x -. curr_point.x);
                      (t.p1.y -. curr_point.y);
                      ((t.p1.x -. curr_point.x)
                         *.(t.p1.x -. curr_point.x)
                         +.(t.p1.y -. curr_point.y)
                         *.(t.p1.y -. curr_point.y)
                      )|];
                  [|(t.p2.x -. curr_point.x);
                    (t.p2.y -. curr_point.y);
                    ((t.p1.x -. curr_point.x)
                        *.(t.p2.x -. curr_point.x)
                        +.(t.p2.y -. curr_point.y)
                        *.(t.p2.y -. curr_point.y)
                     )|];
                  [|(t.p3.x -. curr_point.x);
                    (t.p3.y -. curr_point.y);
                    ((t.p1.x -. curr_point.x)
                          *.(t.p3.x -. curr_point.x)
                          +.(t.p3.y -. curr_point.y)
                          *.(t.p3.y -. curr_point.y)
                    )|] |]
                 in

  let det = det_3 in_matrix in print_float det;
  if is_counterclockwise (t.p1) (t.p2) (t.p3)
    then det > 0.
    else det < 0.
  ;;

let to_modify_t (t_set:triangle_set) (curr_point:point) =
  let 
