#load

exception End;;
let skel =
  try
      while true do
        try
          let s = Graphics.wait_next_event
                    [Graphics.Button_down; Graphics.Key_pressed]
          in if s.Graphics.keypressed then print_string f_key s.Graphics.key
             else if s.Graphics.button
                  then print_string "test"(*f_mouse s.Graphics.mouse_x s.Graphics.mouse_y*)
        with
             End -> raise End
           |  e  -> f_except e
      done
  with
      End  -> f_end ();;
