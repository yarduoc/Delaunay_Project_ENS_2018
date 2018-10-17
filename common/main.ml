let screen_width = ref 800;;
let screen_height = ref 600;;

type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

(* Functions yet to be imported or implemented *)
let add_point (t:triangle_set) (p:point) = t;;
let sleep k = let x = k + 1 in ();;



(* Triangle set initialisation with the frame triangles *)


let init_triangle_set max_x max_y =
    (* Creation of the initial triangle_set*)
    let p1_max = {x = 0.; y = 0.} in
    let p2_max = {x = 0.; y = max_y} in
    let p3_max = {x = max_x; y = 0.} in
    let p4_max = {x = max_x; y = max_y} in
    let t_set = [ { p1 = p1_max; p2 = p2_max; p3 = p3_max};
                  { p1 = p3_max; p2 = p2_max; p3 = p4_max}
                ]
    in t_set
;;

(*Points methods*)

let make_point a b = {x = a; y = b};;

(*Triangles methods*)

let make_triangle a b c = {p1 = a; p2 = b; p3 = c};;



let delaunay p_set max_x max_y =
    let t_set = ref (init_triangle_set max_x max_y) in
    while p_set != [] do
        let curr_point = List.hd p_set in
        t_set := add_point (!t_set) curr_point
    done;
    t_set
;;
