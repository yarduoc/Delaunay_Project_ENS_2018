#use "../common/pointtriangle.ml";;
#use "../common/alphaset.ml";;
#load "graphics.cma";;

open Graphics;;

let norme x y = sqrt ((x)*.(x) +. (y)*.(y));;

let circumcenter p1 p2 p3 =
  let d= 2.*.(p1.x*.(p2.y-.p3.y) +. p2.x*.(p3.y -. p1.y) +. p3.x*.(p1.y -. p2.y))
  in

  let a = (p1.x*.p1.x +. p1.y*.p1.y) in
  let b = (p2.x*.p2.x +. p2.y*.p2.y) in
  let c = (p3.x*.p3.x +. p3.y*.p3.y) in

  let center_x = (a*.(p2.y -. p3.y) +. b*.(p3.y -. p1.y) +. c*.(p1.y -. p2.y))/.d
  in

  let center_y = (a*.(p3.x -. p2.y) +. b*.(p1.x -. p3.x) +. c*.(p2.x -. p1.x))/.d
  in

  center_x, center_y;;

let circumradius p1 p2 p3 =
  let (center_x,center_y) = circumcenter p1 p2 p3 in
      norme (center_x -. p1.x) (center_y -. p1.y);;


let draw_triangle p1 p2 p3 =
  let (center_x,center_y) = circumcenter p1 p2 p3 in
  let radius = int_of_float(circumradius p1 p2 p3) in
    moveto (int_of_float(p1.x)) (int_of_float(p1.y));
    lineto (int_of_float(p2.x)) (int_of_float(p2.y));
    lineto (int_of_float(p3.x)) (int_of_float(p3.y));
    lineto (int_of_float(p1.x)) (int_of_float(p1.y));
    draw_circle (int_of_float(center_x)) (int_of_float(center_y)) radius;;
