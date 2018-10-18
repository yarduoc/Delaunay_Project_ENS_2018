#load "graphics.cma"
open Graphics
#use "Detection/detec.ml"

(* Global variables of the display module*)

let width  = ref 0;;
let height = ref 0;;


(* Data conversion functions for Graphics compatibility *)

let sleep k =
    for l = 0 to (k*10000000) do 1+1 done
;;

let point_to_int_double point =
( (int_of_float point.x),
  (int_of_float point.y)
);;

let triangle_to_int_array triangle =
[| point_to_int_double triangle.p1;
   point_to_int_double triangle.p2;
   point_to_int_double triangle.p3
|];;

(* Data plotting from int couples *)

let plot_d double_i = match double_i with
    | i1,i2 -> fill_circle i1 i2 5;;

(* Interface functions *)

let init_display width height =
    let param_string = " " ^ (string_of_int width) ^ "x"
                           ^ (string_of_int height) ^ "-0+0"
    in
    open_graph param_string;
    set_color black
;;

let rec draw_point p_set = match p_set with
    | [] -> ()
    | curr_point::remaining_points -> plot_d (point_to_int_double curr_point);
                                      draw_point remaining_points
;;
let draw_triangle t_set =
    let rec draw_triangle_aux t_set = match t_set with
        | [] -> ()
        | tr::t -> (let triangle_i = triangle_to_int_array tr in
                    draw_poly triangle_i;
                    draw_triangle_aux t)
    in
    draw_triangle_aux t_set
;;

let clear_display () = clear_graph();;

let debug t_set newpoint =
    set_color blue;
    draw_triangle t_set;
    set_color red;
    let t_set_to_modify = to_modify_tri t_set newpoint in
    draw_triangle t_set_to_modify
;;
