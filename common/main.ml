open Pointtriangle
open Alphaset
open Display
open Changement

let screen_width = ref 800;;
let screen_height = ref 600;;

(* Functions yet to be imported or implemented *)
let sleep k =
    for l = 0 to (k*10000000) do () done
;;

(* Random function *)

let rand_points nb x_max y_max =
    let sortie = ref (empty ()) in
    for k=0 to nb-1 do
        sortie := (cons !sortie {x = Random.float(x_max); y = Random.float(y_max)})
    done;
    !sortie
;;

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

let test_debug () =
    init_display 1001 801;
    let t_set_to_study = delaunay (rand_points 3 1000. 800.) 1001 801 in
    debug t_set_to_study (make_point 400. 600.);
    t_set_to_study
;;

delaunay_stepwise (rand_points 20 1000. 800.) 1001 801;;
