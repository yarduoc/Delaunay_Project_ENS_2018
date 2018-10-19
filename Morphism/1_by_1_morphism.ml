let switch_point p_set new_point =
    let nrst_point = nearest_point p_set new_point in
    let new_set = cons (del p_set nrst_point) new_point
    in new_set;;

let init_morp_set p_set =
    let res_set = ref (empty()) in
    let init_aux curr_point =
        res_set := (curr_point,false)::(!res_set)
    in iter init_aux p_set;
    !res_set;;

let delaunay_switch p_set1 p_set2 =
    let res_p_set = ref (init_morp_set p_set1) in
    let switched_p_set = ref (empty()) in ();;
