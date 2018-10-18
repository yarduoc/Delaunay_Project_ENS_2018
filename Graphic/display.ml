open Graphics
open Pointtriangle
open Alphaset
open Detec

(* Global variables of the display module*)

let width  = ref 0;;
let height = ref 0;;


(* Data conversion functions for Graphics compatibility *)

let sleep k =
    for l = 0 to (k*10000000) do
        ()
    done
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

let rec draw_point p_set =
    if is_empty p_set
        then ()
    else
        let curr_point = car p_set in
        let remaining_points = cdr p_set in
        begin
            plot_d (point_to_int_double curr_point);
            draw_point remaining_points
        end
;;
let rec draw_triangle t_set =
    if is_empty t_set
        then ()
    else
        let curr_triangle = car t_set in
        let other_triangles = cdr t_set in
        let triangle_i = triangle_to_int_array curr_triangle in
        begin
            draw_poly triangle_i;
            draw_triangle other_triangles
        end
;;

let rec draw_line l_set =
    let t_set = ref (empty()) in
    let draw_line_aux curr_line =
        let p1,p2 = curr_line in
        let curr_t = make_triangle p1 p2 p2 in
        t_set := cons (!t_set) curr_t
    in iter draw_line_aux l_set;
    draw_triangle (!t_set);;


let clear_display () = clear_graph();;

let debug t_set newpoint =
    let t_set_to_modify = to_modify_tri t_set newpoint in
    set_color red;
    draw_triangle t_set_to_modify;
    set_color blue;
    draw_point (cons (empty()) newpoint);
    set_color black;;
