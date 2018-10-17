let screen_width = ref 800;;
let screen_height = ref 600;;

type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

#use "Graphic/display.ml"

(* Functions yet to be imported or implemented *)
let add_point (t:triangle_set) (p:point) = t;;
let sleep k = let x = k + 1 in print_int x;;
let copy (t:point_set) = t;;
let cdr (t:point_set) = ((List.tl t):point_set);;
let car (t:point_set) = (List.hd t);;
let is_empty (t:point_set) = (t = []);;

(* Triangle set initialisation with the frame triangles *)

let init_triangle_set max_x max_y =
    let p1_max = {x = 0.; y = 0.} in
    let p2_max = {x = 0.; y = float_of_int max_y} in
    let p3_max = {x = float_of_int max_x; y = 0.} in
    let p4_max = {x = float_of_int max_x; y = float_of_int max_y} in
    let t_set = [ { p1 = p1_max; p2 = p2_max; p3 = p3_max};
                  { p1 = p3_max; p2 = p2_max; p3 = p4_max}
                ]
    in t_set
;;

let delaunay point_set max_x max_y =
    let t_set = ref (init_triangle_set max_x max_y) in
    let p_set = ref (copy point_set) in
    while not (is_empty !p_set) do
        let curr_point = List.hd !p_set in
        t_set := add_point !t_set curr_point;
        p_set := cdr !p_set
    done;
    t_set
;;

let delaunay_stepwise point_set max_x max_y=
    let t_set = ref (init_triangle_set max_x max_y) in
    let p_set = ref (copy point_set) in
    init_display max_x max_y;
    while not (is_empty !p_set) do
        let curr_point = car !p_set in
        t_set := add_point (!t_set) curr_point;
        p_set := cdr !p_set;
        draw_triangle !(t_set);
        draw_point point_set;
        sleep 1;
    done;
;;
