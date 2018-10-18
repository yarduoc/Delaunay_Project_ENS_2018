

type point = {x: float; y: float};;
type triangle = { p1 : point; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

let make_triangle a b c = {p1 = a; p2 = b; p3 = c};;
let make_point a b = {x = a; y = b};;

let p1 = make_point 0. 1.;;
let p2 = make_point 1. 1.;;
let p3 = make_point 2. 2.;;
let p4 = make_point 1. 0.;;

let t1 = make_triangle p1 p2 p3;;
let t2 = make_triangle p1 p2 p4;;
let t3 = make_triangle p1 p3 p4;;


#use "../common/alphaset.ml";;
#use "../Change/changement.ml";;
#use "matrix.ml";;
#use "detec.ml";;

let p_set = cons (cons (cons (empty()) (p1,p2)) (p2,p3)) (p3,p4) ;;

(*
let test_iter l =
  let p = ref [] in
  let apply_iter x = p:= x::(!p) in
  iter apply_iter l ; !p;;

test_iter [1;2;3;4;5;6];;
*)

(*
let p1 = make_point 0. 0.;;
let p2 = make_point 1. 1.;;
let p3 = make_point 2. 2.;;
let p = (ref [p1;p2;p2;p2;p3;p2;p2;p1;p3;p1]);;

suppress p  p1;;
!p;;

*)
(* test find

let p =  [p1;p2;p2;p2;p3;p2;p2;p1;p3;p1];;

find p p4;;
*)

(*
let t_suppr = cons (cons (empty()) t2) t3;;

suppr_border t_set t_suppr;;

*)

(*
let rec random_triangle n =
  if n = 0 then true
  else let p1 = make_point (Random.float(100.)) (Random.float(100.)) in
       let p2 = make_point (Random.float(100.)) (Random.float(100.)) in
       let p3 = make_point (Random.float(100.)) (Random.float(100.)) in

       let value = ((is_counterclockwise p1 p2 p3) != (is_counterclockwise p1 p3 p2)) in

       value&&(random_triangle (n-1));;

random_triangle 10000;;

*)

(*
double_find p_set (p3,p2);;
*)
