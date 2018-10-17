#use "../common/main.ml";;


let is_counterclockwise (p1:point) (p2:point) (p3:point) =  (*Is correct*)
  let det = (p2.x-.p1.x)*.(p3.y-.p1.y) -. (p2.y -. p1.y)*.(p3.x-.p1.x) in
    det >= 0.
;;

let in_circle (t:triangle) (curr_point:point) =


  det > 0. ;;
