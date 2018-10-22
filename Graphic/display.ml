open Pointtriangle
open Graphics
open Alphaset
open Detec
open Stepbystepmorphism

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


(*Color function*)
let rand_color () =
    let r,g,b = Random.int 256, Random.int 256, Random.int 256 in
    set_color (rgb r g b);;

let rec draw_triangle_r_col t_set =
    if is_empty t_set
        then ()
    else
        let curr_triangle = car t_set in
        let other_triangles = cdr t_set in
        let triangle_i = triangle_to_int_array curr_triangle in
        begin
            rand_color();
            draw_poly triangle_i;
            fill_poly triangle_i;
            draw_triangle_r_col other_triangles
        end
;;

(*Drawing function*)

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


let clear_display () = clear_graph();;
