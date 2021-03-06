let screen_width = ref 800;;
let screen_height = ref 800;;

#load "tools.cma";;
#load "uppertools.cma";;
#load "graphics.cma";;


type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

open Graphics;;
open Alphaset;;
open Pointtriangle;;
open Display;;
open Detec;;
open Changement;;

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
#use "Morphism/2set_morphism.ml"
#use "Graphic/display.ml";;


let ord_points p1 p2 =
    if p1.x = p2.x then
        p1.y < p2.y
    else p1.x < p2.x
;;


(* Functions yet to be imported or implemented *)
let sleep k =
    for l = 0 to (k*10000000) do () done
;;

(* Random function *)

let rand_points nb x_max y_max =
    let sortie = ref (empty ()) in
    for k=0 to nb-1 do
        sortie := (cons (!sortie) (make_point (Random.float(x_max)) (Random.float(y_max))))
    done;
    !sortie
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
    done;
    !t_set
;;

let delaunay_default p_set = delaunay p_set 1000 800;;

let delaunay_switch_set p_morph_set1 p_morph_set2 t =
    assert ( (t <= 1.) && (t >= 0.) );
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


let delaunay_morph_set p_m_set1 p_m_set2 t max_x max_y =
    assert ( (t <= 1.) && (t >= 0.) );
    let p1_max = ({x = 0.; y = 0.},-1) in
    let p2_max = ({x = 0.; y = float_of_int max_y},-2) in
    let p3_max = ({x = float_of_int max_x; y = 0.},-3) in
    let p4_max = ({x = float_of_int max_x; y = float_of_int max_y},-4) in
    let p_set_max = cons (cons (cons (cons (empty()) p1_max) p2_max) p3_max) p4_max in

    let p_morph_set1 = concat p_set_max p_m_set1 in
    let p_morph_set2 = concat p_set_max p_m_set2 in
    let new_coord p1 p2 delta=
        let new_x = (delta*.p1.x) +. ((1.-.delta)*.p2.x) in
        let new_y = (delta*.p1.y) +. ((1.-.delta)*.p2.y) in
        make_point new_x new_y
    in

    let middle_m_set m_set1 m_set2 =
        let res_m_set = ref (empty()) in
        let middle ind =
            let p1 = morph_fun m_set1 ind in
            let p2 = morph_fun m_set2 ind in
            res_m_set := cons !res_m_set ((new_coord (fst p1) (fst p2) 0.5),ind)
        in iter middle (init_ind (length m_set1));
        !res_m_set
    in

    let new_m_set = concat p_set_max (middle_m_set p_morph_set1 p_morph_set2) in
    let middle_m_delaunay = delaunay_default (morph_to_point new_m_set) in
    let res_triangle_set = ref (empty()) in

    let morph_triangle_set curr_triangle =
        let p1 = curr_triangle.p1 in
        let p2 = curr_triangle.p2 in
        let p3 = curr_triangle.p3 in

        let ind1 = snd (reci_morph_fun new_m_set p1) in
        let ind2 = snd (reci_morph_fun new_m_set p2) in
        let ind3 = snd (reci_morph_fun new_m_set p3) in

        let set1_p1 = fst (morph_fun p_morph_set1 ind1) in
        let set1_p2 = fst (morph_fun p_morph_set1 ind2) in
        let set1_p3 = fst (morph_fun p_morph_set1 ind3) in

        let set2_p1 = fst (morph_fun p_morph_set2 ind1) in
        let set2_p2 = fst (morph_fun p_morph_set2 ind2) in
        let set2_p3 = fst (morph_fun p_morph_set2 ind3) in

        let new_p1 = new_coord set1_p1 set2_p1 t in
        let new_p2 = new_coord set1_p2 set2_p2 t in
        let new_p3 = new_coord set1_p3 set2_p3 t in

        let new_triangle = make_triangle new_p1 new_p2 new_p3 in
        res_triangle_set := cons !res_triangle_set new_triangle

    in iter morph_triangle_set  middle_m_delaunay;
    !res_triangle_set
;;


let rset1 = rand_m_points 10 1000. 800.;;
let rset2 = delta_set rset1 250. 1000. 800.;;
let f t = delaunay_morph_set rset1 rset2 t 1000 800;;
init_display 1000 800;;
let g ()= let rec g_aux t = draw_triangle (f t); sleep 1; g_aux (t+.0.05) in
        g_aux 0.;;



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
        done;
        !t_set
    in delaunay_stepwiset
;;


#use "Graphic/mouse_test.ml";;
#use "Graphic/affichage_paul.ml";;
init_display 1001 800;;
let points  = (rand_points 500 1000. 800.);;
draw_triangle (delaunay points 1001 801);;
draw_point points;;

sleep(100);;
