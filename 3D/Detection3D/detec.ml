

let is_counterclockwise (p1:point_3D) (p2:point_3D) (p3:point_3D) =
    let matrice =[|
                    [| ( p2.x -. p1.x); ( p3.x -. p1.x) |];
                    [| ( p2.y -. p1.y); ( p3.y -. p1.y) |]
                 |]
    in
    det_2 matrice >= 0.
;;

let in_circle (tri:triangle_3D) (curr_point:point_3D) =
    let ax = (tri.p1.x -. curr_point.x) and ay = (tri.p1.y -. curr_point.y) in
    let bx = (tri.p2.x -. curr_point.x) and by = (tri.p2.y -. curr_point.y) in
    let cx = (tri.p3.x -. curr_point.x) and cy = (tri.p3.y -. curr_point.y) in
    let in_matrix  = [|
                        [|ax; ay; ax*.ax +. ay*.ay|];
                        [|bx; by; bx*.bx +. by*.by|];
                        [|cx; cy; cx*.cx +. cy*.cy|]
                     |]
    in
    let det = det_3 in_matrix in
    if is_counterclockwise (tri.p1) (tri.p2) (tri.p3)
        then det > 0.
    else det < 0.
;;


let to_modify_tri (tri_set:triangle_3D set) (curr_point:point_3D) =
    let tri_list = ref [] in
    let apply p tri =
        if in_circle tri p then
            tri_list := tri::(!tri_list);
    in iter (apply curr_point) tri_set;
    (!tri_list)
;;
