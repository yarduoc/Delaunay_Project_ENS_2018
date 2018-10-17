#use "common/main.ml"
#use "common/alphaset.ml"

let get_line (tri_set:triangle_set) =
    let result_line_set = ref (empty()) in
    let add_line curr_tri =
        result_line_set := (curr_tri.p1,curr_tri.p2) :: (curr_tri.p2,curr_tri.p3) :: (curr_tri.p3,curr_tri.p1) :: (!result_line_set)
    in iter add_line tri_set; !result_line_set
;;

let border_aux (line_set:(point*point) set) =
    let seen_line_set = ref (empty()) in
    let suppr_line_set = ref (empty()) in
    let result_line_set = ref line_set in
    let find_aux curr_line =
        if  not (find !suppr_line_set curr_line) then
            if find !seen_line_set curr_line then
                suppr_line_set := curr_line::(!suppr_line_set);
        if not (find !seen_line_set curr_line) then
            seen_line_set := curr_line::(!seen_line_set);
    in iter find_aux line_set;
    let suppr_aux curr_line =
        if find !suppr_line_set curr_line then
            suppress result_line_set curr_line
    in iter suppr_aux !result_line_set;
    !result_line_set
;;

let border tri_set = border_aux (get_line tri_set);;

let add_point tri_set new_point = 2;;
