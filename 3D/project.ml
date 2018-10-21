#load "graphics.cma";;

open Graphics;;

#use "../common/pointtriangle.ml";;
#use "pointtriangle3D.ml";;
#use "../common/alphaset.ml";;
#use "Detection3D/matrix.ml";;
#use "Detection3D/detec.ml";;
#use "Change3D/changement.ml";;
#use "Graphic3D/display3D.ml";;

(*
let project_point_3D p_3D = make_point (p_3D.x) (p_3D.y);;

let rec projection (p_set : point set) =

  if is_empty p_set
    then empty()
    else cons (projection (cdr p_set)) (project_point_3D (car p_set))
  ;;

*)

  let screen_width = ref 800;;
  let screen_height = ref 600;;

  (* Functions yet to be imported or implemented *)
  let sleep k =
      for l = 0 to (k*10000000) do () done
  ;;

  (* Random function *)

  let rand_points_3D nb x_max y_max z_max =   (*Is 3D*)
      let sortie = ref (empty ()) in
      let f x  y = 12. *. cos((x*.x +. y*.y)/.4.) /. (3. +. x*.x +. y*.y) in
      for k=0 to nb-1 do
      let x_point = (Random.float(x_max))
      and y_point  = (Random.float(y_max)) in
          sortie := (cons (!sortie) (make_point_3D  x_point y_point (f x_point y_point)))
      done;
      !sortie
  ;;

  (* Triangle set initialisation with the frame triangles *)

  let init_triangle_set_3D max_x max_y max_z =  (*Is 3D*)
      let p1_max = make_point_3D 0. 0. 0. in
      let p2_max = make_point_3D 0. (float_of_int max_y) 0. in
      let p3_max = make_point_3D (float_of_int max_x) 0. 0. in
      let p4_max = make_point_3D (float_of_int max_x) (float_of_int max_y) 0. in
      let t_set =   cons
                    (cons (empty()) (make_triangle_3D p1_max p2_max p3_max))
                    (make_triangle_3D p3_max p2_max p4_max)

      in t_set
  ;;

  let delaunay3D point_set max_x max_y max_z=  (*Is 3D*)
      let t_set = ref (init_triangle_set_3D max_x max_y max_z) in
      let p_set = ref (copy point_set) in
      while not (is_empty !p_set) do
          let curr_point = car !p_set in
          t_set := add_point !t_set curr_point;
          p_set := cdr !p_set
      done;
      !t_set
  ;;

  init_display 1001 800;;
  let points_3D  = (rand_points_3D 500 1000. 800. 100.);;
  let p_set_3D = delaunay3D points_3D 1001 801 100;;


  sleep(100);;

let p_set = rand_points_3D 0 1000. 800. 100. ;;
draw_triangle_3D (delaunay3D p_set 1000 800 100);;
