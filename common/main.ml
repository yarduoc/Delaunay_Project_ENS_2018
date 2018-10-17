#use "Graphic/display.ml"

let screen_width = ref 800;;
let screen_height = ref 600;;

type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

(* Functions yet to be imported or implemented *)
let add_point (t:triangle_set) (p:point) = t;;
let sleep k = let x = k + 1 in print_int x;;
let copy (t:point_set) = t;;
let car (t:point_set) = List.tl t;;

(* Triangle set initialisation with the frame triangles *)

let init_triangle_set max_x max_y =
    let p1_max = {x = 0.; y = 0.} in
    let p2_max = {x = 0.; y = max_y} in
    let p3_max = {x = max_x; y = 0.} in
    let p4_max = {x = max_x; y = max_y} in
    let t_set = [ { p1 = p1_max; p2 = p2_max; p3 = p3_max};
                  { p1 = p3_max; p2 = p2_max; p3 = p4_max}
                ]
    in t_set
;;

let delaunay point_set max_x max_y =
    let t_set = ref (init_triangle_set max_x max_y) in
    let p_set = ref copy point_set in
    while p_set != [] do
        let curr_point = List.hd p_set in
        t_set := add_point (!t_set) curr_point;
        p_set :=
    done;
    t_set
;;

let delaunay_stepwise point_set max_x max_y=
    let t_set = ref (init_triangle_set max_x max_y) in
    let p_set = ref copy point_set in
    init_display (int_of_float max_x) (int_of_float max_y);
    while p_set != [] do
        let curr_point = List.hd p_set in
        t_set := add_point (!t_set) curr_point;
        draw_triangle !(t_set);
        draw_point point_set;
        sleep 1;
    done;
;;
