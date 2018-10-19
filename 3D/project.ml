#use "../common/pointtriangle.ml";;
#use "pointtriangle3D.ml";;
#use "../common/alphaset.ml";;

let project_point_3D p_3D = make_point (p_3D.x) (p_3D.y);;

let rec projection (p_set : point set) =

  if is_empty projection
    then empty()
    else cons (projection (cdr p_set)) (project_point_3D (car p_set))
  ;;


  let screen_width = ref 800;;
  let screen_height = ref 600;;

  (* Functions yet to be imported or implemented *)
  let sleep k =
      for l = 0 to (k*10000000) do () done
  ;;

  (* Random function *)

  let rand_points_3D nb x_max y_max z_max =   (*Is 3D*)
      let sortie = ref (empty ()) in
      for k=0 to nb-1 do
          sortie := (cons (!sortie) (make_point_3D (Random.float(x_max)) (Random.float(y_max))) (Random.float(z_max)))
      done;
      !sortie
  ;;

  (* Triangle set initialisation with the frame triangles *)

  let init_triangle_set_3D max_x max_y max_z =  (*Is 3D*)
      let p1_max = make_point_3D 0. 0. 0. in
      let p2_max = make_point_3D 0.(float_of_int max_y) 0. in
      let p3_max = make_point_3D (float_of_int max_x) 0. 0. in
      let p4_max = make_point_3D (float_of_int max_x) (float_of_int max_y) 0. in
      let t_set =   cons
                    (cons (empty()) (make_triangle_3D p1_max p2_max p3_max))
                    (make_point_3D p3_max p2_max p4_max)

      in t_set
  ;;

  let delaunay3D point_set max_x max_y max_z=  (*Is 3D*)
      let t_set = ref (init_triangle_set max_x max_y max_z) in
      let p_set = ref (copy point_set) in
      while not (is_empty !p_set) do
          let curr_point = car !p_set in
          t_set := add_point !t_set curr_point;
          p_set := cdr !p_set
      done;
      !t_set
  ;;

  let delaunay_stepwise point_set max_x max_y=
      let t_set = ref (init_triangle_set max_x max_y) in
      let p_set = ref (copy point_set) in
      init_display max_x max_y;
      while not (is_empty !p_set) do
          clear_display ();
          let curr_point = car !p_set in
          t_set := add_point (!t_set) curr_point;
          p_set := cdr !p_set;
          draw_triangle !(t_set);
          draw_point point_set;
      done;
      !t_set
  ;;

  let test_debug n =
      init_display 1001 801;
      let max_x = 1001 in
      let max_y = 801 in

      let point_set = (rand_points n 1000. 800.) in
      let delaunay_stepwiset =
          let t_set = ref (init_triangle_set max_x max_y) in
          let p_set = ref (copy point_set) in
          init_display max_x max_y;
          while not (is_empty !p_set) do
              clear_display ();
              let curr_point = car !p_set in
              t_set := add_point (!t_set) curr_point;
              p_set := cdr !p_set;
              draw_triangle !(t_set);
              draw_point point_set;
              debug (!t_set) (car !p_set);
          done;
          !t_set
      in delaunay_stepwiset
  ;;

  init_display 1001 800;;
  let points_3D  = (rand_points_3D 500 1000. 800. 100.);;
  let p_set_3D = delaunay_3D points_3D 1001 801 100;;


  sleep(100);;
