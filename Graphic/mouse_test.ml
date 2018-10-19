#load "graphics.cma"
open Graphics

exception End;;
let wait_move p_set =
    let t_set = ref (empty ()) in
    try
    while true do
        let pressed = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
        if pressed.button then
            let mouse_origin = (make_point (float_of_int pressed.Graphics.mouse_x) (float_of_int pressed.Graphics.mouse_y)) in
            let point_a_deplacer = ref (nearest_point !p_set mouse_origin) in
            if sqr_dist !point_a_deplacer mouse_origin < 100. then
                begin
                p_set := del !p_set !point_a_deplacer;
                t_set := (delaunay !p_set 1000 800);
                p_set := ord_insert ord_points !p_set !point_a_deplacer;
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
                                        draw_triangle affiche;
                                        draw_point !p_set;
                                    end
                done;
                end;
        else
            begin
                let mouse_pos = make_point (float_of_int pressed.Graphics.mouse_x) (float_of_int pressed.Graphics.mouse_y) in
                if pressed.key = '+' then
                    begin
                    p_set := ord_insert ord_points !p_set mouse_pos;
                    raise End
                    end;
                if pressed.key = '-' then
                    begin
                        let point_a_deplacer = ref (nearest_point !p_set mouse_pos) in
                        if sqr_dist !point_a_deplacer mouse_pos < 100. then
                            begin
                                p_set := del !p_set !point_a_deplacer;
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
        draw_triangle (delaunay !p_set 1000 800);
        draw_point !p_set;
        wait_move p_set;
        sound 440 2;
    done
;;
