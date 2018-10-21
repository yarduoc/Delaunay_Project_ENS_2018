#load "graphics.cma"
open Graphics

exception End;;

let wait_move t =
    try
    while true do
        let pressed = Graphics.wait_next_event [Graphics.Key_pressed] in
        begin
            if pressed.key = '+' then
                (t:= min (!t +. 0.1) 1.; print_string "test1"; print_newline ();raise End;);
            if pressed.key = '-' then
                (t:= max (!t -. 0.1) 0.; print_string "test1"; print_newline ();raise End;);
            if pressed.key = 'q' then
                (close_graph(); print_string "test1"; print_newline ();raise End;);
            raise End
        end
    done;
    with End -> ()
;;

let run_morph p_set1 =
    let t = ref 0. in
    let p_set2 = delta_set p_set1 200. 1000. 800. in
    open_graph " 1000x800-0+0";
    while true do
        set_color black;
        clear_graph ();
        draw_triangle (delaunay_switch_set p_set1 p_set2 !t);
        set_color blue;
        draw_point (morph_to_point p_set1);
        set_color green;
        draw_point (morph_to_point p_set2);
        set_color black;
        wait_move t;
    done
;;
