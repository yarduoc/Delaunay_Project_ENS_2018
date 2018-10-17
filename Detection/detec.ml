#use "../common/main.ml";;


let is_counterclockwise (p1:point) (p2:point) (p3:point) =  (*Is correct*)
  let det = (p2.x-.p1.x)*.(p3.y-.p1.y) -. (p2.y -. p1.y)*.(p3.x-.p1.x) in
    det >= 0.
;;

let in_circle (t:triangle) (curr_point:point) =
  let a_11 = in
  let a_12 = in
  let a_13 = in
  let a_21 = in
  let a_22 = in
  let a_23 = in
  let a_31 = in
  let a_32 = in
  let a_33 = in
  let c_12 = in
  let c_22 = in
  let c_32 = in
  let det =   a_11*.c_12 +. a_22*.c_22 +. a_32*.c_32 in

  det > 0. ;;
