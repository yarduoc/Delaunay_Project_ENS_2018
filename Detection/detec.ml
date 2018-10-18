let is_counterclockwise (p1:point) (p2:point) (p3:point) =  (*Is correct*)
  let matrice =[| [|(p2.x -. p1.x); (p3.x -. p1.x)|];
                  [|(p2.y -. p1.y); (p3.y -. p1.y)|]|] in
    det_2 matrice >= 0.
;;

let in_circle (tri:triangle) (curr_point:point) =
  let ax = (tri.p1.x -. curr_point.x) and ay = (tri.p1.y -. curr_point.y) in
  let bx = (tri.p2.x -. curr_point.x) and by = (tri.p2.y -. curr_point.y) in
  let cx = (tri.p3.x -. curr_point.x) and cy = (tri.p3.y -. curr_point.y) in
  (*let in_matrix =  [| [| (tri.p1.x -. curr_point.x);
                      (tri.p1.y -. curr_point.y);
                      ((tri.p1.x -. curr_point.x)
                         *.(tri.p1.x -. curr_point.x)
                         +.(tri.p1.y -. curr_point.y)
                         *.(tri.p1.y -. curr_point.y)
                      )|];
                  [|(tri.p2.x -. curr_point.x);
                    (tri.p2.y -. curr_point.y);
                    ((tri.p2.x -. curr_point.x)
                        *.(tri.p2.x -. curr_point.x)
                        +.(tri.p2.y -. curr_point.y)
                        *.(tri.p2.y -. curr_point.y)
                     )|];
                  [|(tri.p3.x -. curr_point.x);
                    (tri.p3.y -. curr_point.y);
                    ((tri.p2.x -. curr_point.x)
                          *.(tri.p3.x -. curr_point.x)
                          +.(tri.p3.y -. curr_point.y)
                          *.(tri.p3.y -. curr_point.y)
                    )|] |]*)

      let in_matrix  = [| [|ax; ay; ax*.ax +. ay*.ay|];
                          [|bx; by; bx*.bx +. by*.by|];
                          [|cx; cy; cx*.cx +. cy*.cy|]|]
                 in

  let det = det_3 in_matrix in
  if is_counterclockwise (tri.p1) (tri.p2) (tri.p3)
    then det > 0.
    else det < 0.
  ;;


let to_modify_tri (tri_set:triangle set) (curr_point:point) =
  let tri_list = ref [] in
  let apply p tri =
        if in_circle tri p
        then tri_list := tri::(!tri_list);
    in iter (apply curr_point) tri_set;
    (!tri_list);;

(*

let next tri_set tri =
  let tri_list = ref [tri] in
  let apply tri curr_triangle =
    let have_common_arc = ((tri.p1 = curr_triangle.p1) && ((tri.p2= curr_triangle.p2)||(tri.p2 = curr_triangle.p3)||(tri.p3= curr_triangle.p2)||(tri.p3 = curr_triangle.p3))) ||
                          ((tri.p1= curr_triangle.p2) && ((tri.p2= curr_triangle.p1)||(tri.p2 = curr_triangle.p3)||(tri.p3= curr_triangle.p1)||(tri.p3 = curr_triangle.p3))) ||
                          ((tri.p1 = curr_triangle.p3)&& ((tri.p2= curr_triangle.p1)||(tri.p2 = curr_triangle.p2)||(tri.p3= curr_triangle.p1)||(tri.p3 = curr_triangle.p2)))||
                          ((tri.p2 = curr_triangle.p1) && ((tri.p1= curr_triangle.p2)||(tri.p1 = curr_triangle.p3)||(tri.p3= curr_triangle.p2)||(tri.p3 = curr_triangle.p3))) ||
                          ((tri.p2= curr_triangle.p2) && ((tri.p1= curr_triangle.p1)||(tri.p1 = curr_triangle.p3)||(tri.p3= curr_triangle.p1)||(tri.p3 = curr_triangle.p3)) )||
                          ((tri.p2 = curr_triangle.p3)&& ((tri.p1= curr_triangle.p1)||(tri.p1 = curr_triangle.p2)||(tri.p3= curr_triangle.p1)||(tri.p3 = curr_triangle.p2)))||
                          ((tri.p3 = curr_triangle.p1) && ((tri.p2= curr_triangle.p2)||(tri.p2 = curr_triangle.p3)||(tri.p1= curr_triangle.p2)||(tri.p1 = curr_triangle.p3)) )||
                          ((tri.p3= curr_triangle.p2) && ((tri.p2= curr_triangle.p1)||(tri.p2 = curr_triangle.p3)||(tri.p1= curr_triangle.p1)||(tri.p1 = curr_triangle.p3))) ||
                          ((tri.p3 = curr_triangle.p3)&& ((tri.p2= curr_triangle.p1)||(tri.p2 = curr_triangle.p2)||(tri.p1= curr_triangle.p1)||(tri.p1 = curr_triangle.p2)))
      in
      if have_common_arc
      then tri_list := curr_triangle::(!tri_list)
    in iter (apply tri) tri_set;
      (!tri_list);;


let to_modify_tri (tri_set:triangle set) (curr_point:point) =
  let null_point = {x=0.;y=0.} in
  let in_tri = ref {p1 = null_point; p2 = null_point; p3 = null_point} in
  let apply p tri =
        if in_circle tri p
        then in_tri := tri
    in iter (apply curr_point) tri_set;
    next (tri_set) (!in_tri);;

*)
