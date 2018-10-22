let screen_width = ref 800;;
let screen_height = ref 600;;

Random.self_init ();;

#use "common/alphaset.ml";;
#use "common/pointtriangle.ml";;
#use "Detection/matrix.ml";;
#use "Detection/detec.ml";;
#use "Change/changement.ml";;
#use "Morphism/1_by_1_morphism.ml";;
#use "Morphism/2set_morphism.ml";;
#use "Graphic/display.ml";;




(* Functions yet to be imported or implemented *)
let sleep k = let p = ref 0 in
    for l = 0 to (k*10000000) do p:= 1 done
;;

(* Random function *)

let rand_points nb x_max y_max =
    let ord p1 p2 =
        if p1.x = p2.x then
            p1.y < p2.y
        else p1.x < p2.x
    in
    let sortie = ref [] in
    for k=0 to nb-1 do
        sortie := (make_point (Random.float(x_max)) (Random.float(y_max)))::(!sortie)
    done;
    sort ord !sortie
;;

let rand_m_points nb x_max y_max =
    point_set_to_m_point_set (rand_points nb x_max y_max);;

(* Triangle set initialisation with the frame triangles *)

let init_triangle_set max_x max_y =
    let p1_max = {x = 0.; y = 0.} in
    let p2_max = {x = 0.; y = float_of_int max_y} in
    let p3_max = {x = float_of_int max_x; y = 0.} in
    let p4_max = {x = float_of_int max_x; y = float_of_int max_y} in
    let t_set =   cons
                  (cons (empty()) {p1 = p1_max; p2 = p2_max; p3 = p3_max})
                  { p1 = p3_max; p2 = p2_max; p3 = p4_max}

    in t_set
;;

let delaunay point_set max_x max_y =
    let t_set = ref (init_triangle_set max_x max_y) in
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
        sleep 5;
    done;
;;

let delaunay_default p_set = delaunay p_set 800 600;;
(*
let delaunay_switch_set p_morph_set1 p_morph_set2 t =
    assert ( (t <= 1.) && (t >= 0.) );
    let res_p_set = ref (empty()) in
    let ind_set = init_ind (length p_morph_set1) in
    let switch_set_aux curr_ind =
        let p1 = fst (morph_fun p_morph_set1 curr_ind) in
        let p2 = fst (morph_fun p_morph_set2 curr_ind) in
        let new_x = (t*.p1.x) +. ((1.-.t)*.p2.x) in
        let new_y = (t*.p1.y) +. ((1.-.t)*.p2.y) in
        let new_point = make_point new_x new_y in
        res_p_set := cons !res_p_set new_point
    in
    iter switch_set_aux ind_set;
    delaunay_default !res_p_set
;;
*)
(*
let delaunay_morph_set p_m_set1 p_m_set2 t max_x max_y =
    assert ( (t <= 1.) && (t >= 0.) );
    let p1_max = ({x = 0.; y = 0.},-1) in
    let p2_max = ({x = 0.; y = float_of_int max_y},-2) in
    let p3_max = ({x = float_of_int max_x; y = 0.},-3) in
    let p4_max = ({x = float_of_int max_x; y = float_of_int max_y},-4) in
    let p_set_max = cons (cons (cons (cons (empty()) p1_max) p2_max) p3_max) p4_max in
    let p_morph_set1 = concat p_set_max p_m_set1 in
    let p_morph_set2 = concat p_set_max p_m_set2 in
    (*Donne les coordonnées d'un point intermediaire à deux points*)
    let new_coord p1 p2 delta=
        let new_x = (delta*.p1.x) +. ((1.-.delta)*.p2.x) in
        let new_y = (delta*.p1.y) +. ((1.-.delta)*.p2.y) in
        make_point new_x new_y
    in
    (*Donne le m_set intermédiaire*)
    let middle_m_set m_set1 m_set2 =
        let res_m_set = ref (empty()) in
        let middle ind =
            let p1 = morph_fun m_set1 ind in
            let p2 = morph_fun m_set2 ind in
            res_m_set := cons !res_m_set ((new_coord (fst p1) (fst p2) 0.5),ind)
        in iter middle (init_ind (length m_set1));
        !res_m_set
    in
    (*middle_m_set + points extremaux*)
    let new_m_set = concat p_set_max (middle_m_set p_morph_set1 p_morph_set2) in
    (*delaunay du middle set*)
    let middle_m_delaunay = delaunay_default (morph_to_point new_m_set) in
    let res_triangle_set = ref (empty()) in
    (*foction qui a un triangle associe le nouveau*)
    let morph_triangle_set curr_triangle =
        let p1 = curr_triangle.p1 in
        let p2 = curr_triangle.p2 in
        let p3 = curr_triangle.p3 in
        let ind1 = snd (reci_morph_fun new_m_set p1) in
        let ind2 = snd (reci_morph_fun new_m_set p2) in
        let ind3 = snd (reci_morph_fun new_m_set p3) in
        let set1_p1 = fst (morph_fun p_morph_set1 ind1) in
        let set1_p2 = fst (morph_fun p_morph_set1 ind2) in
        let set1_p3 = fst (morph_fun p_morph_set1 ind3) in
        let set2_p1 = fst (morph_fun p_morph_set2 ind1) in
        let set2_p2 = fst (morph_fun p_morph_set2 ind2) in
        let set2_p3 = fst (morph_fun p_morph_set2 ind3) in
        let new_p1 = new_coord set1_p1 set2_p1 t in
        let new_p2 = new_coord set1_p2 set2_p2 t in
        let new_p3 = new_coord set1_p3 set2_p3 t in
        let new_triangle = make_triangle new_p1 new_p2 new_p3 in
        res_triangle_set := cons !res_triangle_set new_triangle
    in iter morph_triangle_set  middle_m_delaunay;
    !res_triangle_set
;;
*)

let delaunay_morph_set mp_set1 mp_set2 t max_x max_y =
    (*verification de la bonne valeur de point list*)
    assert ( (t <= 1.) && (t >= 0.) );
    (* adds maximum points to the point list *)
    let p1 = (make_morph_point 0. 0. (-1)) in
    let p2 = (make_morph_point 0. max_y (-2)) in
    let p3 = (make_morph_point max_x 0. (-3)) in
    let p4 = (make_morph_point max_x max_y (-4)) in
    let mp_set_max = cons (cons (cons (cons (empty()) p1) p2) p3) p4 in

    let new_mp_set1 = concat mp_set_max mp_set1 in
    let new_mp_set2 = concat mp_set_max mp_set2 in

    (*Donne les coordonnées d'un point intermediaire à deux points*)
    let middle_point p1 p2 t =
        let new_x = (t *. p1.mx) +. ((1. -. t) *. p2.mx) in
        let new_y = (t *. p1.my) +. ((1. -. t) *. p2.my) in
        make_morph_point new_x new_y p1.label
    in
    (*Donne le mp_set intermédiaire*)
    let middle_mp_set m_p_set1 m_p_set2 =
        let new_mp_set = ref (empty()) in
        for label = 1 to length m_p_set1 do
            let p1 = point_matching_label m_p_set1 label in
            let p2 = point_matching_label m_p_set2 label in
            new_mp_set := cons !new_mp_set (middle_point p1 p2 0.5)
        done;
        !new_mp_set
    in
    (*middle_mp_set + points extremaux*)
    let new_mp_set = concat mp_set_max (middle_mp_set mp_set1 mp_set2) in
    (*delaunay du middle set*)
    let middle_mp_delaunay = delaunay_default (morph_set_to_point_set new_mp_set) in
    let triangle_set = ref (empty()) in
    (*foction qui a un triangle associe le nouveau*)
    let get_triangle_set curr_triangle =
        let p1 = curr_triangle.p1 in
        let p2 = curr_triangle.p2 in
        let p3 = curr_triangle.p3 in

        let label1 = (point_matching_coordinates new_mp_set p1).label in
        let label2 = (point_matching_coordinates new_mp_set p2).label in
        let label3 = (point_matching_coordinates new_mp_set p3).label in

        let set1_p1 = point_matching_label new_mp_set1 label1 in
        let set1_p2 = point_matching_label new_mp_set1 label2 in
        let set1_p3 = point_matching_label new_mp_set1 label3 in


        let set2_p1 = point_matching_label new_mp_set2 label1 in
        let set2_p2 = point_matching_label new_mp_set2 label2 in
        let set2_p3 = point_matching_label new_mp_set2 label3 in

        let new_p1 = middle_point set1_p1 set2_p1 t in
        let new_p2 = middle_point set1_p2 set2_p2 t in
        let new_p3 = middle_point set1_p3 set2_p3 t in

        let new_triangle = make_triangle (morph_point_to_point new_p1) (morph_point_to_point new_p2) (morph_point_to_point new_p3) in
        triangle_set := cons !triangle_set new_triangle

    in iter get_triangle_set middle_mp_delaunay;
    !triangle_set
;;


let rset1 = rand_m_points 100 800. 600.;;
let rset2 = delta_set rset1 50. 800. 600.;;
let f t = delaunay_morph_set rset1 rset2 t 800. 600.;;
init_display 800 600;;
let g ()=
    while true do
        for k = 0 to 20 do
            let t = (float_of_int k) /. 20. in
            let to_draw = f t in
            clear_display ();
            draw_triangle to_draw;
            sleep 0;
        done;
        for k = 20 downto 0 do
            let t = (float_of_int k) /. 20. in
            let to_draw = f t in
            clear_display ();
            draw_triangle to_draw;
            sleep 0;
        done;
        let s = Graphics.wait_next_event [Key_pressed;Poll] in
        if s.keypressed then failwith "sortie_de_boucle";
    done
;;
#use "Graphic/mouse_test.ml";;


let color_delaunay_morph_set cmp_set1 cmp_set2 t x_max y_max =
    assert ( (t <= 1.) && (t >= 0.) );
    let mp_set1,color_set1 = cmp_set1 in
    let mp_set2,color_set2 = cmp_set2 in
    let tri_set = delaunay_morph_set mp_set1 mp_set2 t x_max y_max in

    (* adds maximum points to the point list *)
    let p1 = (make_morph_point 0. 0. (-1)) in
    let p2 = (make_morph_point 0. max_y (-2)) in
    let p3 = (make_morph_point max_x 0. (-3)) in
    let p4 = (make_morph_point max_x max_y (-4)) in
    let mp_set_max = cons (cons (cons (cons (empty()) p1) p2) p3) p4 in

    let new_mp_set1 = concat mp_set_max mp_set1 in
    let new_mp_set2 = concat mp_set_max mp_set2 in

    (*Donne les coordonnées d'un point intermediaire à deux points*)
    let middle_point p1 p2 t =
        let new_x = (t *. p1.mx) +. ((1. -. t) *. p2.mx) in
        let new_y = (t *. p1.my) +. ((1. -. t) *. p2.my) in
        make_morph_point new_x new_y p1.label
    in
    (* Color management*)
    let middle_color col1 col2 t =
        let r1,g1,b1 = col1 in
        let r2,g2,b2 = col2 in
        let new_r = (t * r1) + ((1-t) * r2) in
        let new_g = (t * g1) + ((1-t) * g2) in
        let new_b = (t * b1) + ((1-t) * b2) in
        new_r,new_g,new_b;
    in

    let avg_triple col1 col2 col3 =
    let r1,g1,b1 = col1 in
    let r2,g2,b2 = col2 in
    let r3,g3,b3 = col3 in
    (r1+r2+r3)/3,(g1+g2+g3)/3,(b1+b2+b3)/3;

    in
    (*Donne le mp_set intermédiaire*)
    let middle_mp_set m_p_set1 m_p_set2 =
        let new_mp_set = ref (empty()) in
        for label = 1 to length m_p_set1 do
            let p1 = point_matching_label m_p_set1 label in
            let p2 = point_matching_label m_p_set2 label in
            new_mp_set := cons !new_mp_set (middle_point p1 p2 0.5)
        done;
        !new_mp_set
    in
    (*middle_mp_set + points extremaux*)
    let new_mp_set = concat mp_set_max (middle_mp_set mp_set1 mp_set2) in
    (*delaunay du middle set*)
    let middle_mp_delaunay = delaunay_default (morph_set_to_point_set new_mp_set) in
    let morph_ctriangle_set = ref (empty()) in
    (*foction qui a un triangle associe le nouveau*)
    let get_triangle_set curr_triangle =
        let p1 = curr_triangle.p1 in
        let p2 = curr_triangle.p2 in
        let p3 = curr_triangle.p3 in

        let label1 = (point_matching_coordinates new_mp_set p1).label in
        let label2 = (point_matching_coordinates new_mp_set p2).label in
        let label3 = (point_matching_coordinates new_mp_set p3).label in

        let set1_p1 = point_matching_label new_mp_set1 label1 in
        let set1_p2 = point_matching_label new_mp_set1 label2 in
        let set1_p3 = point_matching_label new_mp_set1 label3 in

        let set2_p1 = point_matching_label new_mp_set2 label1 in
        let set2_p2 = point_matching_label new_mp_set2 label2 in
        let set2_p3 = point_matching_label new_mp_set2 label3 in

        let set1_c1 = color_matching_label color_set1 label1 in
        let set1_c2 = color_matching_label color_set1 label2 in
        let set1_c3 = color_matching_label color_set1 label3 in

        let set2_c1 = color_matching_label color_set2 label1 in
        let set2_c2 = color_matching_label color_set2 label2 in
        let set2_c3 = color_matching_label color_set2 label3 in

        let new_c1 = middle_color set1_c1 set2_c1 t in
        let new_c2 = middle_color set1_c2 set2_c2 t in
        let new_c3 = middle_color set1_c3 set2_c3 t in

        let new_color = avg_triple new_c1 new_c2 new_c3 in

        let new_p1 = middle_point set1_p1 set2_p1 t in
        let new_p2 = middle_point set1_p2 set2_p2 t in
        let new_p3 = middle_point set1_p3 set2_p3 t in

        let new_triangle = make_triangle (morph_point_to_point new_p1) (morph_point_to_point new_p2) (morph_point_to_point new_p3) in
        morph_ctriangle_set := cons !morph_triangle_set (new_triangle,new_color)

    in iter get_triangle_set  middle_mp_delaunay;
    !morph_ctriangle_set
;;
