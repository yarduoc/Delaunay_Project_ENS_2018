(* m  stands for morphism*)

open Pervasives;;

let pi = 3.14159265;;

(*init index list
let init_ind l =
    let res_ind_set = ref (empty()) in
    for curr_ind = l downto 1 do
        res_ind_set := cons !res_ind_set curr_ind
    done;
    !res_ind_set
;; *)

(* transphormation entre m_point et point*)
(*
let point_to_morph p_set =
    let res_set = ref (empty()) in
    let ind = ref 1 in
    let init_aux curr_point =
        res_set := (curr_point,!ind)::(!res_set);
        ind := 1 + !ind;
    in iter init_aux p_set;
    !res_set
;; *)
let point_set_to_m_point_set p_set =
    let label = ref 0 in
    let incremental_indexation point =
        label := !label + 1;
        point_to_morph_point point !label
    in
    map incremental_indexation p_set
;;

(*
let morph_to_point morph_p_set =
    let res_set_point = ref (empty()) in
    let morph_to_point_aux curr_morph_p_set =
        let curr_p = fst curr_morph_p_set in
        res_set_point := cons (!res_set_point) curr_p
    in iter morph_to_point_aux morph_p_set;
    !res_set_point
;;
*)
let morph_set_to_point_set mp_set =
    map morph_point_to_point mp_set
;;




(*fonction qui donne le point associé à un index*)
(*
let morph_fun morph_set ind =
    let res_m_point = ref (car morph_set) in
    let morph_aux curr_point =
        let curr_ind = snd curr_point in
        if curr_ind = ind then res_m_point := curr_point;
    in iter morph_aux morph_set;
    !res_m_point;; *)
exception Notin of (morph_point set);;

let point_matching_label mp_set label =
    let match_label morph_point = (morph_point.label = label) in
    try
        find_pred match_label mp_set
    with _ -> (print_int label;  raise (Notin mp_set);)
;;

(* fonction qui à des coordonnées donne le m_point*)
(*
let reci_morph_fun morph_set v_point =
    let res_m_point = ref (car morph_set) in
    let b = ref false in
    let morph_aux curr_m_point =
        let curr_point = fst curr_m_point in
        if curr_point = v_point then (res_m_point := curr_m_point; b:= true)
    in iter morph_aux morph_set;
    if !b then
        !res_m_point
    else failwith "point not in set";;
*)

let point_matching_coordinates mp_set point =
    let x, y = point.x, point.y in
    let match_coords morph_point =  morph_point.mx = x
                                     && morph_point.my = y
    in
    find_pred (match_coords) mp_set
;;

(*fonction qui a un mp_set crée une variation*)
(*
let delta_set p_set delta x_max y_max =
    let res_p_set = ref (empty()) in
    let delta_aux curr_m_point =
        let curr_point,ind = curr_m_point in
        let angle = Random.float (2.*.pi) in
        let dist = Random.float delta in
        let x,y = curr_point.x,curr_point.y in
        let new_x = min ( max 1. ((cos angle)*.dist+.x) ) (x_max-.1.)  in
        let new_y = min ( max 1. ((sin angle)*.dist+.y) ) (y_max-.1.)  in
        let new_point = make_point new_x new_y in
        res_p_set := cons !res_p_set (new_point,ind)
    in iter delta_aux p_set;
    !res_p_set
;; *)

let delta_set mp_set delta x_max y_max =
    let get_delta_shifted_point m_point =
        let angle    = Random.float (2. *. pi) in
        let distance = Random.float delta in
        let x = m_point.mx in
        let y = m_point.my in
        let delta_x = (cos angle) *. distance +. x in
        let delta_y = (sin angle) *. distance +. y in
        let new_x = min (max 1 (x + delta_x)) ((float_of_int x_max) - 1.) in
        let new_y = min (max 1 (y + delta_y)) ((float_of_int y_max) - 1.) in
        make_morph_point new_x new_y m_point.label
    in map get_delta_shifted_point mp_set
;;
