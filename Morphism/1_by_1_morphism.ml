(* m  stands for morphism*)

open Pervasives;;

let pi = 3.14159265;;

let init_morp_set p_set =
    let res_set = ref (empty()) in
    let ind = ref 1 in
    let init_aux curr_point =
        res_set := (curr_point,!ind)::(!res_set);
        ind := 1 + !ind;
    in iter init_aux p_set;
    !res_set
;;

let morph_to_point m_p =
    let res_set_point = ref (empty()) in
    let m_p_to_p_aux curr_m_p =
        let curr_p = fst curr_m_p in
        res_set_point := cons (!res_set_point) curr_p
    in iter m_p_to_p_aux m_p;
    !res_set_point;;



let nearest_morph_point p_morph_set mouse_point =
    let res_point = ref (make_point (-.1.) (-.1.) ) in
    let inf = ref (max_float) in
    let is_min curr_point =
        let p = fst(curr_point) in
        let b = snd(curr_point) in
        if b && (sqr_dist p mouse_point) < (!inf) then begin
            inf := sqr_dist p mouse_point;
            res_point := p
        end
    in iter is_min p_morph_set;
    !res_point
;;


let delta_set p_set =
    let res_p_set = ref (empty()) in
    let delta_aux curr_m_point =
        let curr_point,ind = curr_m_point in
        let angle = Random.float (2.*.pi) in
        let dist = Random.float 100. in
        let x,y = curr_point.x,curr_point.y in
        let new_point = make_point (((cos angle)*.dist)+.x) (((sin angle)*.dist)+.y)
        in res_p_set := cons !res_p_set (new_point,ind)
    in iter delta_aux p_set;
    !res_p_set;;

let switch_set p_morph_set1 p_morph_set2 t =
