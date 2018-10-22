(*open Pointtriangle
open Alphaset
open Matrix*)

(*Is the point in a circumcircle ?*)

let is_counterclockwise (p1:point) (p2:point) (p3:point) =
    let matrice =[|
                    [| ( p2.x -. p1.x); ( p3.x -. p1.x) |];
                    [| ( p2.y -. p1.y); ( p3.y -. p1.y) |]
                 |]
    in
    det_2 matrice >= 0.
;;

let in_circle (tri:triangle) (curr_point:point) =
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


let to_modify_tri (tri_set:triangle set) (curr_point:point) =
    let tri_list = ref [] in
    let apply p tri =
        if in_circle tri p then
            tri_list := tri::(!tri_list);
    in iter (apply curr_point) tri_set;
    (!tri_list)
;;

(*Where is the nearest point of the mouse ? *)

let sqr_dist p1 p2 =
    (p1.x -. p2.x)**2. +. (p1.y -. p2.y)**2.
;;

let nearest_point p_set mouse_point =
    let res_point = ref (car p_set) in
    let inf = ref (sqr_dist (car p_set) mouse_point) in
    let is_min curr_point =
        if (sqr_dist curr_point mouse_point) < (!inf) then begin
            inf := sqr_dist curr_point mouse_point;
            res_point := curr_point
        end
    in iter is_min p_set;
    !res_point
;;


let nearest_morph_point morph_p_set mouse_point =
    let res_point = ref (make_point (-.1.) (-.1.) ) in
    let inf = ref (max_float) in
    let is_min curr_point =
        let p = fst(curr_point) in
        let b = snd(curr_point) in
        if b && (sqr_dist p mouse_point) < (!inf) then begin
            inf := sqr_dist p mouse_point;
            res_point := p
        end
    in iter is_min morph_p_set;
    if !inf = max_float then
        failwith "no more point"
    else
        !res_point
;;
