open Alphaset

type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point set;;
type triangle_set = triangle set;;


let make_point a b = {x = a; y = b};;
let get_x (p:point) = p.x;;
let get_y (p:point) = p.y;;

let make_triangle a b c = {p1 = a; p2 = b; p3 = c};;
let get_first_point tri = tri.p1;;
let get_second_point tri = tri.p2;;
let get_third_point tri = tri.p3;;
