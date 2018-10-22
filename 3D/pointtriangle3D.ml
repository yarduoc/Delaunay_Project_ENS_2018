type point_3D = {x: float; y: float; z:float};;
type triangle_3D = { p1 : point_3D; p2 : point_3D ; p3 : point_3D};;
type point_3D_set = point_3D list;;
type triangle_3D_set = triangle_3D list;;

let make_triangle_3D a b c = {p1 = a; p2 = b; p3 = c};;
let make_point_3D a b c = {x = a; y = b; z = c};;
