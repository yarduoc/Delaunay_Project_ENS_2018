let init_morp_set p_set =
    let res_set = ref (empty()) in
    let init_aux curr_point =
        res_set := (curr_point,true)::(!res_set)
    in iter init_aux p_set;
    !res_set
;;


let nearest_morph_point p_morph_set mouse_point =
    let res_point = ref (make_point (-.1.) (-.1.) ) in
    let inf = ref (max_float) in
    let is_min curr_point =
        let p = fst(curr_point) in
        let b = snd(curr_point) in
        if b && (sqr_dist p mouse_point) < (!inf) then begin
            inf := sqr_dist curr_point mouse_point;
            res_point := curr_point
        end
    in iter is_min p_set;
    !res_point
;;



let switch_point p_set new_point =
    let nrst_point = nearest_morph_point p_set new_point in
    let new_set = cons (del p_set nrst_point) new_point
    in new_set
;;


let delaunay_switch p_set1 p_set2 =
    let res_p_set = ref (init_morp_set p_set1) in
    let aux_switch p_set2 =
;;
