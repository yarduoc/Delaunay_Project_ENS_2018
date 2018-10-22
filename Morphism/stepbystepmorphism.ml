(* m  stands for morphism*)

open Pervasives
open Alphaset
open Pointtriangle

let pi = 3.14159265;;

let point_set_to_m_point_set p_set =
    let label = ref 0 in
    let incremental_indexation point =
        label := !label + 1;
        point_to_morph_point point !label
    in
    map incremental_indexation p_set
;;

let morph_set_to_point_set mp_set =
    map morph_point_to_point mp_set
;;

let point_matching_label mp_set label =
    let match_label morph_point = (morph_point.label = label) in

        find_pred match_label mp_set
  ;;


let point_matching_coordinates mp_set point =
    let x, y = point.x, point.y in
    let match_coords morph_point =  morph_point.mx = x
                                     && morph_point.my = y
    in
    find_pred (match_coords) mp_set
;;

let delta_set mp_set delta x_max y_max =
    let get_delta_shifted_point m_point =
        let angle    = Random.float (2. *. pi) in
        let distance = Random.float delta in
        let x = m_point.mx in
        let y = m_point.my in

        let delta_x = (cos angle) *. distance in
        let delta_y = (sin angle) *. distance in
        let new_x = min (max 1. (x +. delta_x)) (x_max -. 1.) in
        let new_y = min (max 1. (y +. delta_y)) (y_max -. 1.) in

        make_morph_point new_x new_y m_point.label
    in map get_delta_shifted_point mp_set
;;
