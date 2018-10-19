#load "graphics.cma"
open Graphics

exception End;;

let wait_move t =
    try
    while true do
        let pressed = Graphics.wait_next_event [Graphics.Button_down] in
        begin
            if pressed.key = '+' then
                (t:= min (!t +. 0.1) 1.; raise End;);
            if pressed.key = '-' then
                (t:= max (!t -. 0.1) 0.; raise End;);
            if pressed.key = 'q' then
                (close_graph(); raise End;);
            raise End
        end
    done;
    with End -> ()
;;

let run p_set1 =
    let t = ref 0. in
    let p_set2 = delta_set p_set1 200. in
    while true do
        set_color black;
        clear_graph ();
        draw_triangle (delaunay_switch_set p_set1 p_set2 !t);
        draw_point (morph_to_point p_set1);
        draw_point (morph_to_point p_set2);
        wait_move t;
    done
;;
