#load "graphics.cma"
open Graphics
#use "../common/main.ml"

(* Global variables of the display module*)

let width  = ref 0;;
let height = ref 0;;
let update_counter = ref 0;;
let points = ref [];;
let triangles = ref [];;


(* Data conversion functions for Graphics compatibility *)


let point_to_int_double point = ( (int_of_float point.x),
                                  (int_of_float point.y)
                                );;

let triangle_to_int_array triangle = [| point_to_int_double triangle.p1;
                                        point_to_int_double triangle.p2;
                                        point_to_int_double triangle.p3
                                     |];;

(* Data plotting function *)

let plot_d double_i = match double_i with
    | i1,i2 -> plot i1 i2;;

let delaunay_step () =


let init_graph width height =
    let param_string = " " ^ (string_of_int width) ^ "x" in
    let param_string = param_string ^ (string_of_int height) ^ "-0+0"in
    print_string param_string;
    open_graph param_string;
    set_color black;;


let rec draw_point p_set = match p_set with
    | [] -> ()
    | curr_point::remaining_points -> plot_d (point_to_int_double curr_point);
                                      draw_point remaining_points
;;


let rec draw_triangle t_set = match t_set with
    | [] -> ()
    | tr::t -> let triangle_i = triangle_to_int_array tr in
               draw_poly triangle_i;
               draw_triangle t
;;

let delaunay_stepwise (x:point_set) = ();;
