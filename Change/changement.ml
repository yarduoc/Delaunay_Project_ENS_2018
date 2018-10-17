#use "Change/main.ml"
#use "common/alphaset.ml"

let get_line tri_set =
    let result_line_set = ref (empty()) in
    let add_line cur_tri =
        result_line_set := (cur_tri.p1,cur_tri.p2) :: (cur_tri.p2,cur_tri.p3) :: (cur_tri.p3,cur_tri.p1) :: (!result_line_set)
    in iter add_line tri_set;;


let border_aux line_set =
    let ots_set_line = ref (empty()) in
    let sup_set_line = ref (empty()) in
    let result_set_line = ref line_set in
    let find_aux cur_line =
        if  not (find !sup_set_line cur_line) then
            if find cur_line (!ots_set_line) then
                sup_set_line := cur_line::(!sup_set_line);
        if not (find !ots_set_line cur_line) then
            ots_set_line := cur_line::(!sup_set_line);
    in iter find_aux line_set;
    let sup_aux cur_line =
        if find !sup_set_line cur_line then
            suppress result_set_line cur_line
    in iter sup_aux result_set_line;
    result_set_line;;

let border tri_set = border_aux (get_line tri_set);;

let add_point tri_set new_point = 2;;
