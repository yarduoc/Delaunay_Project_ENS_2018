#load "graphics.cma"
open Graphics
#use "common/main.ml"
#use "common/alphaset.ml"

let x = 800 ;;
open_graph (" " ^ (string_of_int x ) ^ "x600-0+0");;

let nearest_point p_set curr_coordinates =
    make_point ((car p_set).x) curr_coordinates.y
;;

let print_coords event =
    print_string "mouse [";
    print_int event.Graphics.mouse_x;
    print_string " ; ";
    print_int event.Graphics.mouse_y;
    print_string "]";
    print_newline ()
;;

exception End;;
let skel () =
    try
    while true do
        let pressed = Graphics.wait_next_event [Graphics.Button_down]
        in
        print_string "pressed :";
        print_coords pressed;
        while true do
            let released = Graphics.wait_next_event [Graphics.Button_up]
            in
            print_string "released :";
            print_coords released;
            draw_poly_line [|
                             (pressed.Graphics.mouse_x,pressed.Graphics.mouse_y);
                             (released.Graphics.mouse_x,released.Graphics.mouse_y)
                           |];
            raise End
        done;
    done;
    with End -> ()
;;
let run () =
    while true do skel () done
;;
