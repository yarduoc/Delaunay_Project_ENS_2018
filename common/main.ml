let screen_width = 800;;
let screen_height = 600;;

type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;


(*Points methods*)

let make_point a b = {x = a; y = b};;

(*Triangles methods*)

let make_triangle a b c = {p1 = a; p2 = b; p3 = c};;
