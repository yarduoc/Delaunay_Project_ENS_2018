(*© Copyright Paul Bastide, Alex Coudray, Lauric Desauw 25/04/2019 CC-BY 4.0*)

#load "graphics.cma"
open Graphics
#use "Graphic/display.ml"
#use "common/pointtriangle.ml"


let number_of_points = ref 0;;
let m_points_data = ref empty ();;

let draw_interest_point m_point =
    moveto m_point.mx m_point.my;
    plot_d m_point.mx m_point.my;
    draw_string (string_of_int m_point.label)
;;

let draw_m_points mp_set = iter draw_interest_point mp_set;;

let add_m_point x y =
    number_of_points := !number_of_points + 1;
    let new_point = make_m_point x y !number_of_points in
    m_points_data := cons !m_points_data newpoint
;;

let decrease_label m_point = make_m_point m_point.x m_point.y (m_point.label - 1);;

let delete_m_point point =
    if find m_points_data point then
        begin
            del m_points_data point;
            map (function x -> if ord_m_points point x then decrease_label x else x) m_points_data;
            number_of_points := !number_of_points - 1;
        end
;;


let ord_m_points a b = if a.label < b.label then true else false ;;


exception End;;
let wait_move mp_set =
    try
    while true do
        let pressed = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
        if pressed.button then
            let mouse_origin = (make_point (float_of_int pressed.Graphics.mouse_x) (float_of_int pressed.Graphics.mouse_y)) in
            let point_a_deplacer = ref (nearest_point !m_p_set mouse_origin) in
            if sqr_dist !point_a_deplacer mouse_origin < 100. then
                while true do
                    let released = Graphics.wait_next_event [Graphics.Button_up; Graphics.Mouse_motion] in
                    let mouse_destination = (make_point (float_of_int released.Graphics.mouse_x) (float_of_int released.Graphics.mouse_y)) in
                    p_set := del !p_set !point_a_deplacer;
                    p_set := ord_insert ord_points !p_set mouse_destination;
                    match released with
                        | status when not status.button -> raise End
                        | status when released.Graphics.mouse_x < 0 ||
                                      released.Graphics.mouse_x > 1000 ||
                                      released.Graphics.mouse_y < 0 ||
                                      released.Graphics.mouse_y > 800
                                      ->
                                      p_set := ord_insert ord_points !p_set !point_a_deplacer;
                                      p_set := del !p_set mouse_destination;
                        | status -> begin
                                        let affiche = add_point !t_set mouse_destination in
                                        point_a_deplacer := mouse_destination;
                                        clear_graph ();
                                        draw_m_point !mp_set;
                                    end
                done;
        else
            begin
                if pressed.key = '+' then
                    begin
                        add_m_point pressed.Graphics.mouse_x pressed.Graphics.mouse_y;
                        raise End
                    end;
                if pressed.key = '-' then
                    begin
                        let point_a_deplacer = ref (nearest_point !p_set mouse_pos) in
                        if sqr_dist !point_a_deplacer mouse_pos < 100. then
                            begin
                                delete_m_point point_a_deplacer;
                                raise End
                            end;
                    end;
                if pressed.key = 'q' then
                    close_graph();
            end
    done;
    with End -> ()
;;
let run point_set =
    let p_set = ref point_set in
    while true do
        set_color black;
        clear_graph ();
        draw_triangle (delaunay !p_set 800 600);
        draw_point !p_set;
        wait_move p_set;
    done
;;
