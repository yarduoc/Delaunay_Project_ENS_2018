
(* Global variables of the display module*)

let pi = 3.1415926535;;

(* Data conversion functions for Graphics compatibility *)


let point_to_int_triple point =
( (int_of_float point.x),
  (int_of_float point.y),
  (int_of_float point.z)
);;

let triangle_to_int_array triangle =
[| point_to_int_triple triangle.p1;
   point_to_int_triple triangle.p2;
   point_to_int_triple triangle.p3
|];;

let point_to_float_triple point =
( (point.x),
  (point.y),
  (point.z)
);;

let triangle_to_float_array triangle =
[| point_to_float_triple triangle.p1;
   point_to_float_triple triangle.p2;
   point_to_float_triple triangle.p3
|];;

let point_to_int_double point =
( (int_of_float point.x),
  (int_of_float point.y)
);;

let projected_triangle_to_int_array triangle =
[| point_to_int_double triangle.p1;
   point_to_int_double triangle.p2;
   point_to_int_double triangle.p3
|];;

(* Data plotting from int couples *)

let plot_d triple_i = match triple_i with
    | i1,i2,_ -> fill_circle i1 i2 5;;

(* Interface functions *)

let init_display width height =
    let param_string = " " ^ (string_of_int width) ^ "x"
                           ^ (string_of_int height) ^ "-0+0"
    in
    open_graph param_string;
    set_color black
;;

let rec draw_point_3D p_set =
    if is_empty p_set
        then ()
    else
        let curr_point = car p_set in
        let remaining_points = cdr p_set in
        begin
            plot_d (point_to_int_triple curr_point);
            draw_point_3D remaining_points
        end
;;

let not_extreme_p point max_x max_y =
  let p1_max = make_point_3D 0. 0. 0. in
  let p2_max = make_point_3D 0. (float_of_int max_y) 0. in
  let p3_max = make_point_3D (float_of_int max_x) 0. 0. in
  let p4_max = make_point_3D (float_of_int max_x) (float_of_int max_y) 0. in

    not ((point = p1_max) ||
    (point = p2_max) ||
    (point = p3_max) ||
    (point = p4_max))
    ;;

let not_extreme_tri tri max_x max_y =
    (not_extreme_p tri.p1 max_x max_y) &&
    (not_extreme_p tri.p2 max_x max_y) &&
    (not_extreme_p tri.p3 max_x max_y);;



let rec slope_function t_set angle_set max_x max_y=
  if is_empty t_set
    then
        let a_max = maxi (!angle_set) in
        let a_min = mini (!angle_set) in

          let f_slope point_array =
        begin
          let p1 = point_array.(0) in
          let p2 = point_array.(1) in
          let p3 = point_array.(2) in

          let vect1 = vect p1 p2 in
          let vect2 = vect p1 p3 in
          let uz = (0., 0., 1.) in

          let normale = prod_vect vect1 vect2 in

          let angle =  (asin (norme (prod_vect uz normale) /. (norme normale))) in
           cos ((angle -. a_min) *. pi /. (2. *. (a_max -. a_min)))
        end;
        in
        (f_slope)

    else
    begin
      if not_extreme_tri (car t_set) max_x max_y
      then
      begin
        let point_array = (triangle_to_float_array (car t_set)) in
        let p1 = point_array.(0) in
        let p2 = point_array.(1) in
        let p3 = point_array.(2) in

        let vect1 = vect p1 p2 in
        let vect2 = vect p1 p3 in
        let uz = (0., 0., 1.) in

        let normale = prod_vect vect1 vect2 in

        angle_set:= cons (!angle_set)
                         (asin (norme (prod_vect uz normale) /. (norme normale)))
      end;

      slope_function (cdr t_set) angle_set max_x max_y

    end

  ;;

let draw_triangle_set_3D t_set max_x max_y =
  let angle_set = ref (empty()) in
  let f_slope = slope_function t_set angle_set max_x max_y in
  let rec draw_triangle_3D t_set =
  begin
      if is_empty t_set
          then ()
      else
          let curr_triangle = car t_set in
          let other_triangles = cdr t_set in

          if not_extreme_tri curr_triangle max_x max_y
          then
            let projected_triangle_i = projected_triangle_to_int_array curr_triangle in
            let slope_factor = f_slope (triangle_to_float_array curr_triangle) in
            let part_red = 255 - (int_of_float(slope_factor*.255.)) in
            let color = rgb part_red 0 0 in
            begin
                set_color (color);
                draw_poly  projected_triangle_i;
                fill_poly  projected_triangle_i;
                draw_triangle_3D other_triangles
            end;

          else
          draw_triangle_3D other_triangles
    end;
    in
    synchronize ();
    draw_triangle_3D t_set
;;

let rec draw_line l_set =
    let t_set = ref (empty()) in
    let draw_line_aux curr_line =
        let (p1,p2) = curr_line in
        let curr_t = make_triangle_3D p1 p2 p2 in
        t_set := cons (!t_set) curr_t
    in iter draw_line_aux l_set;
    draw_triangle_set_3D (!t_set) 800 600 ;
    synchronize ()
;;


let clear_display () = clear_graph();;
