#use "../common/alphaset.ml";;
#use "../common/main.ml";;
#use "matrix.ml"

let is_counterclockwise (p1:point) (p2:point) (p3:point) =  (*Is correct*)
  let det = (p2.x-.p1.x)*.(p3.y-.p1.y) -. (p2.y -. p1.y)*.(p3.x-.p1.x) in
    det >= 0.
;;

let in_circle (tri:triangle) (curr_point:point) =
  let in_matrix =  [| [| (tri.p1.x -. curr_point.x);
                      (tri.p1.y -. curr_point.y);
                      ((tri.p1.x -. curr_point.x)
                         *.(tri.p1.x -. curr_point.x)
                         +.(tri.p1.y -. curr_point.y)
                         *.(tri.p1.y -. curr_point.y)
                      )|];
                  [|(tri.p2.x -. curr_point.x);
                    (tri.p2.y -. curr_point.y);
                    ((tri.p1.x -. curr_point.x)
                        *.(tri.p2.x -. curr_point.x)
                        +.(tri.p2.y -. curr_point.y)
                        *.(tri.p2.y -. curr_point.y)
                     )|];
                  [|(tri.p3.x -. curr_point.x);
                    (tri.p3.y -. curr_point.y);
                    ((tri.p1.x -. curr_point.x)
                          *.(tri.p3.x -. curr_point.x)
                          +.(tri.p3.y -. curr_point.y)
                          *.(tri.p3.y -. curr_point.y)
                    )|] |]
                 in

  let det = det_3 in_matrix in print_float det;
  if is_counterclockwise (tri.p1) (tri.p2) (tri.p3)
    then det > 0.
    else det < 0.
  ;;


let to_modify_tri (tri_set:triangle_set) (curr_point:point) =
  let tri_list = ref [] in
  let apply p tri =
        if in_circle tri p
        then tri_list := tri::(!tri_list)
    in iter (apply curr_point) tri_set;
    (!tri_list);;
