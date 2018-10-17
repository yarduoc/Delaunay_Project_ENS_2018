#load "graphics.cma"
open Graphics
#use "../common/main.ml"

let p = {x = float; y = float};;

type int_point = {i_x: int; i_y: int};;
type int_triangle = { i_p1 : int_point; i_p2 : int_point ; i_p3 : int_point};;

let point_to_int_point po = { i_x = (int_of_float po.x);
                                 i_y = (int_of_float po.y)
                               };;

let triangle_to_int_triangle triangle = { i_p1 = point_to_int_point triangle.p1;
                                          i_p2 = point_to_int_point triangle.p2;
                                          i_p3 = point_to_int_point triangle.p3;
                                        };;


let init_graph width height =
    close_graph();
    set_color "black";
    let param_string = " " ^ (string_of_int width) ^ "x" in
    let param_string = param_string ^ (string_of_int height) ^ "-0+0"in
    open_graph param_string;;

let rec draw_point (x:point_set) = match point_set with
    | [] -> ()
    | p::t -> plot (int_of_float p.x) (int_of_float p.y);
             draw_point t
;;

let draw_triangle (x:triangle_set) = match triangle_set with
    | [] -> ()
    | p::t -> ()

;;
let delaunay_stepwise (x:point_set) = ();;
