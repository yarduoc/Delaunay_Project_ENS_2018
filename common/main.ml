(*© Copyright Paul Bastide, Alex Coudray, Lauric Desauw 25/04/2019 CC-BY 4.0*)

let screen_width = ref 800;;
let screen_height = ref 600;;

Random.self_init ();;

#use "common/alphaset.ml";;
#use "common/pointtriangle.ml";;
#use "Detection/matrix.ml";;
#use "Detection/detec.ml";;
#use "Change/changement.ml";;
#use "Morphism/1_by_1_morphism.ml";;
#use "Graphic/display.ml";;

(* Random function *)

let rand_points nb x_max y_max =
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
        synchronize ();
        sleep 1;
    done;
;;

let delaunay_default p_set = delaunay p_set 800 600;;

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
    let morph_triangle_set = ref (empty()) in
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
        morph_triangle_set := cons !morph_triangle_set new_triangle

    in iter get_triangle_set  middle_mp_delaunay;
    !morph_triangle_set
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
            synchronize ();
        done;
        for k = 20 downto 0 do
            let t = (float_of_int k) /. 20. in
            let to_draw = f t in
            clear_display ();
            draw_triangle to_draw;
            synchronize ();
        done;
        let s = Graphics.wait_next_event [Key_pressed;Poll] in
        if s.keypressed then failwith "sortie_de_boucle";
    done
;;

#use "Graphic/dynamic_display.ml";;
