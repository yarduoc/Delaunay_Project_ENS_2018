let prod_vect (x1,x2,x3) (y1,y2,y3) =
  let z1 = x2*.y3 -. x3*.y2 in
  let z2 = x3*.y1 -. x1*.y3 in
  let z3 = x1*.y2 -. x2*.y1 in

  (z1,z2,z3);;

let norme (x,y,z) = sqrt ((x)*.(x) +. (y)*.(y) +. z*.z);;

let vect (a1,a2,a3) (b1,b2,b3) =
    (b1 -. a1, b2 -. a2, b3 -. a3);;

let maxi a_set =
  let rec max_aux b_set m =
    if is_empty b_set
    then m

    else
    begin
      if (car b_set) > m
        then max_aux (cdr b_set) (car b_set)
        else max_aux (cdr b_set) m
    end;
    in
    max_aux a_set (car a_set);;

let mini a_set =
  let rec min_aux b_set m =
    if is_empty b_set
    then m

    else
    begin
      if (car b_set) < m
        then min_aux (cdr b_set) (car b_set)
        else min_aux (cdr b_set) m
    end;
    in
    min_aux a_set (car a_set);;
(*
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
*)
