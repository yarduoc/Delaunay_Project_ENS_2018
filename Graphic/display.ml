#load "graphics.cma"
open Graphics
#use "../common/main.ml"

(*
type int_point = {i_x: int; i_y: int};;
type int_triangle = { i_p1 : int_point; i_p2 : int_point ; i_p3 : int_point};;

let point_to_int_point po = { i_x = (int_of_float po.x);
                                 i_y = (int_of_float po.y)
                               };;

let triangle_to_int_triangle triangle = { i_p1 = point_to_int_point triangle.p1;
                                          i_p2 = point_to_int_point triangle.p2;
                                          i_p3 = point_to_int_point triangle.p3;
                                        };; *)

let point_to_int_double point = ( (int_of_float point.x),
                                  (int_of_float point.y)
                                );;

let triangle_to_int_array triangle = [| point_to_int_double triangle.p1;
                                        point_to_int_double triangle.p2;
                                        point_to_int_double triangle.p3
                                     |];;

let plot_d double_i = match double_i with
    | i1,i2 -> plot i1 i2;;

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
