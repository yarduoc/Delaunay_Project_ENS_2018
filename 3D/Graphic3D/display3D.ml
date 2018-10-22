
(* Global variables of the display module*)

let width  = ref 0;;
let height = ref 0;;


(* Data conversion functions for Graphics compatibility *)

let sleep k =
    for l = 0 to (k*10000000) do
        ()
    done
;;

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


let slope_function t_set =
  let angle_set = (ref empty()) in

  let rec slope_triangle tri_set =
    if is_empty tri_set
    then
        let a_max = maxi (!angle_set) in
        let a_min = mini (!angle_set) in


        let f_slope point_array =
       (let p1 = point_array.(0) in
        let p2 = point_array.(1) in
        let p3 = point_array.(2) in

        let vect1 = vect p1 p2 in
        let vect2 = vect p1 p3 in
        let uz = (0., 0., 1.) in

        let normale = prod_vect vect1 vect2 in

        let angle =  (asin (norme (prod_vect uz normale) /. (norme normale))) in

        (angle -. a_min) /. (a_max -. a_min)) in


        f_slope;

    else
      begin
      let point_array = (triangle_to_float_array (car t_set)) in
      let p1 = point_array.(0) in
      let p2 = point_array.(1) in
      let p3 = point_array.(2) in

      let vect1 = vect p1 p2 in
      let vect2 = vect p1 p3 in
      let uz = (0., 0., 1.) in

      let normale = prod_vect vect1 vect2 in

      angle_set:= cons (!angle_set)  (asin (norme (prod_vect uz normale) /. (norme normale)))

      end;


  (slope_triangle t_set)


      ;;





let draw_triangle_set_3D t_set =
  let f_slope = slope_function t_set in
  begin
    let rec draw_triangle_3D t_set =
      if is_empty t_set
          then ()
          else
          let curr_triangle = car t_set in
          let other_triangles = cdr t_set in
          let projected_triangle_i = projected_triangle_to_int_array curr_triangle in
          let slope = f_slope (triangle_to_float_array curr_triangle) in
          let part_red = 255 - (int_of_float(slope_factor*.255.)) in
          let color = rgb part_red 0 0 in
          begin
              set_color (color);
              draw_poly  projected_triangle_i;
              fill_poly  projected_triangle_i;
              draw_triangle_3D other_triangles
          end;
    end;

    draw_triangle_3D t_set
;;

let rec draw_line l_set =
    let t_set = ref (empty()) in
    let draw_line_aux curr_line =
        let (p1,p2) = curr_line in
        let curr_t = make_triangle_3D p1 p2 p2 in
        t_set := cons (!t_set) curr_t
    in iter draw_line_aux l_set;
    draw_triangle_3D (!t_set);;


let clear_display () = clear_graph();;

let debug t_set newpoint =
    let t_set_to_modify = to_modify_tri t_set newpoint in
    set_color red;
    draw_triangle_3D t_set_to_modify;
    set_color blue;
    draw_point_3D (cons (empty()) newpoint);
    set_color black;;
