#load "graphics.cma"
open Graphics

exception End;;
let wait_move p_set =
    try
    while true do
        let pressed = Graphics.wait_next_event [Graphics.Button_down] in
        let mouse_origin = (make_point (float_of_int pressed.Graphics.mouse_x) (float_of_int pressed.Graphics.mouse_y)) in
        let point_a_deplacer = nearest_point !p_set mouse_origin in

        if sqr_dist point_a_deplacer mouse_origin < 100. then set_color green
        else set_color red;
        draw_point (cons (empty ()) point_a_deplacer);
        while true do
            let released = Graphics.wait_next_event [Graphics.Button_up] in
                if sqr_dist point_a_deplacer mouse_origin < 100. then
                    begin
                    p_set := del !p_set point_a_deplacer;
                    p_set := ord_insert ord_points !p_set (make_point (float_of_int released.Graphics.mouse_x) (float_of_int released.Graphics.mouse_y));
                    end;
            raise End
        done;
    done;
    with End -> ()
;;
let run point_set =
    let p_set = ref point_set in
    while true do
        set_color black;
        clear_graph ();
        draw_triangle (delaunay !p_set 1000 800);
        draw_point !p_set;
        wait_move p_set;
    done
;;
