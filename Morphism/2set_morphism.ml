
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
