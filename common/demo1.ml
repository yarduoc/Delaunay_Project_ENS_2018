let screen_width = ref 800;;
let screen_height = ref 600;;

type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

let make_triangle a b c = {p1 = a; p2 = b; p3 = c};;
let make_point a b = {x = a; y = b};;

#use "alphaset.ml"
#use "../Detection/matrix.ml"
#use "../Detection/detec.ml"
#use "../Change/changement.ml"
#use "../Graphic/display.ml"

#use "main.ml";;

delaunay_stepwise (rand_points 100 1000. 800.) 1001 801;;
sleep 100;;
