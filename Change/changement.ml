open Alphaset
open Pointtriangle
open Detec

let double_find p_set p = let p1,p2 = p in (find p_set p || find p_set (p2,p1)) ;;

let get_line (tri_set:triangle_set) =
    let result_line_set = ref (empty()) in
    let add_line curr_tri =
        result_line_set := cons ( cons ( cons (!result_line_set) (curr_tri.p1,curr_tri.p2) ) (curr_tri.p2,curr_tri.p3) ) (curr_tri.p3,curr_tri.p1)
    in iter add_line tri_set; !result_line_set
;;

let border_aux (line_set:(point*point) set) =
    let seen_line_set = ref (empty()) in
    let suppr_line_set = ref (empty()) in
    let result_line_set = ref line_set in
    let find_aux curr_line =
    if  not (double_find !suppr_line_set curr_line) then
        if double_find !seen_line_set curr_line then
            suppr_line_set := cons (!suppr_line_set) curr_line ;
    if not (double_find !seen_line_set curr_line) then
        seen_line_set := cons (!seen_line_set)  curr_line
    in iter find_aux line_set;
    let suppr_aux curr_line =
        if double_find !suppr_line_set curr_line then
            suppress result_line_set curr_line
    in iter suppr_aux !result_line_set;
    !result_line_set
;;

let border tri_set = border_aux (get_line tri_set);;

let suppr_border tri_set suppr_set =
    let t_set = ref tri_set in
    let suppr_border_aux curr_tri =
        suppress t_set (curr_tri)
    in iter suppr_border_aux suppr_set;
    !t_set
;;


let add_point tri_set new_point =
    let modify_tri_set = to_modify_tri tri_set new_point in
    let new_border = border modify_tri_set in
    let result_tri_set = ref (suppr_border tri_set modify_tri_set) in
    let add_tri_aux curr_line =
        let p1,p2 = curr_line in
        let new_tri = make_triangle p1 p2 new_point in
        result_tri_set := cons (!result_tri_set) new_tri
    in
    iter add_tri_aux new_border;
    !result_tri_set
;;
