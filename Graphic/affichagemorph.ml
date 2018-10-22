open Graphics
open Display
open Delaunay
open Stepbystepmorphism


exception End;;

let wait_move t =
    try
    while true do
        let pressed = Graphics.wait_next_event [Graphics.Key_pressed] in
        begin
            if pressed.key = '+' then
                (t:= min (!t +. 0.1) 1.; print_string "test1"; print_newline ();raise End);
            if pressed.key = '-' then
                (t:= max (!t -. 0.1) 0.; print_string "test1"; print_newline ();raise End);
            if pressed.key = 'q' then
                (close_graph(); print_string "test1"; print_newline ();raise End)
        end
    done;
    with End -> ()
;;

let run_morph p_set1 =
    let t = ref 0.0 in
    let p_set2 = delta_set p_set1 100. 1000. 800. in
    open_graph " 1000x800-0+0";
    while true do
        set_color black;
        let to_draw = delaunay_morph_set p_set1 p_set2 !t 1000. 800. in
        clear_graph ();
        draw_triangle (to_draw);
        set_color blue;
        draw_point (morph_set_to_point_set p_set1);
        set_color green;
        draw_point (morph_set_to_point_set p_set2);
        set_color black;
        wait_move t;
    done
;;
