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
