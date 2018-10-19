let screen_width = ref 800;;
let screen_height = ref 600;;

#use "common/alphaset.ml"


type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

let make_triangle a b c = {p1 = a; p2 = b; p3 = c};;
let make_point a b = {x = a; y = b};;

let print_point point =
    print_string "(";
    print_float point.x;
    print_string ";";
    print_float point.y;
    print_string ")"
;;

let print_triangle t =
    print_string "Triangle : p1 = ";
    print_point t.p1;
    print_string " p2 = ";
    print_point t.p2;
    print_string " p3 = ";
    print_point t.p3;
    print_newline ()
;;

let print_triangle_set t_set = iter print_triangle t_set;;

#use "Detection/matrix.ml"
#use "Detection/detec.ml"
#use "Detection/ext_detect.ml"
#use "Change/changement.ml"
#use "Morphism/1_by_1_morphism.ml"
#use "Graphic/display.ml";;


let ord_points p1 p2 =
    if p1.x = p2.x then
        p1.y < p2.y
    else p1.x < p2.x
;;


(* Functions yet to be imported or implemented *)
let sleep k = let p = ref 0 in
    for l = 0 to (k*10000000) do p:= 1 done
;;

(* Random function *)

let rand_points nb x_max y_max =
    let ord p1 p2 =
        if p1.x = p2.x then
            p1.y < p2.y
        else p1.x < p2.x
    in
    let sortie = ref [] in
    for k=0 to nb-1 do
        sortie := (make_point (Random.float(x_max)) (Random.float(y_max)))::(!sortie)
    done;
    sort ord !sortie
;;

let rand_m_points nb x_max y_max =
    point_to_morph (rand_points nb x_max y_max);;

(* Triangle set initialisation with the frame triangles *)

let init_triangle_set max_x max_y =
    let p1_max = {x = 0.; y = 0.} in
    let p2_max = {x = 0.; y = float_of_int max_y} in
    let p3_max = {x = float_of_int max_x; y = 0.} in
    let p4_max = {x = float_of_int max_x; y = float_of_int max_y} in
    let t_set =   cons
                  (cons (empty()) {p1 = p1_max; p2 = p2_max; p3 = p3_max})
                  { p1 = p3_max; p2 = p2_max; p3 = p4_max}

    in t_set
;;

let delaunay point_set max_x max_y =
    let t_set = ref (init_triangle_set max_x max_y) in
    let p_set = ref (copy point_set) in
    while not (is_empty !p_set) do
        let curr_point = car !p_set in
        t_set := add_point !t_set curr_point;
        p_set := cdr !p_set
    done;
    !t_set
;;

let delaunay_stepwise point_set max_x max_y=
    let t_set = ref (init_triangle_set max_x max_y) in
    let p_set = ref (copy point_set) in
    init_display max_x max_y;
    while not (is_empty !p_set) do
        clear_display ();
        let curr_point = car !p_set in
        t_set := add_point (!t_set) curr_point;
        p_set := cdr !p_set;
        draw_triangle !(t_set);
        draw_point point_set;
        sleep 5;
    done;
;;

let delaunay_default p_set = delaunay p_set 1000 800;;

let delaunay_switch_set p_morph_set1 p_morph_set2 t =
    let res_p_set = ref (empty()) in
    let ind_set = init_ind (length p_morph_set1) in
    let switch_set_aux curr_ind =
        let p1 = fst (morph_fun p_morph_set1 curr_ind) in
        let p2 = fst (morph_fun p_morph_set2 curr_ind) in
        let new_x = (t*.p1.x) +. ((1.-.t)*.p2.x) in
        let new_y = (t*.p1.y) +. ((1.-.t)*.p2.y) in
        let new_point = make_point new_x new_y in
        res_p_set := cons !res_p_set new_point
    in
    iter switch_set_aux ind_set;
    delaunay_default !res_p_set;;

let test_debug n =
    init_display 1001 801;
    let max_x = 1001 in
    let max_y = 801 in

    let point_set = (rand_points n 1000. 800.) in
    let delaunay_stepwiset =
        let t_set = ref (init_triangle_set max_x max_y) in
        let p_set = ref (copy point_set) in
        init_display max_x max_y;
        while not (is_empty !p_set) do
            clear_display ();
            let curr_point = car !p_set in
            t_set := add_point (!t_set) curr_point;
            p_set := cdr !p_set;
            draw_triangle !(t_set);
            draw_point point_set;
            debug (!t_set) (car !p_set);
            sleep 10;
        done;
        !t_set
    in delaunay_stepwiset
;;

#use "Graphic/mouse_test.ml";;
