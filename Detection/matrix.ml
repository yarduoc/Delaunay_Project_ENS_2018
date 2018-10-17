type 'a matrix = 'a list list;;

let index l i = (*give the i-eme element of l*)
  let rec aux l i k = match l with
    |[] -> failwith "index"
    |t::q -> if i = k then t else aux l i (k+1);

    in aux l i 0;;

let indice m i j = index (index m i) j;; (* give M[i,j]*)

let rec shape m = match m with
  |t::q ->  1 + shape q
  |_ -> 0;;

let mineur m  i j = (*give the minor of m develop with M[i,j]*)
  


let det_2 m =
    (indice m 1 1) *.(indice m 2 2) -. (indice m 1 2) *.(indice m 2 1)
    ;;

let det_3 m =
  let c_12 = -(mineur m 1 2)in
  let c_22 = (mineur m 2 2)in
  let c_32 = - (mineur m 3 2)in
  (indice m 1 2)*.c_12 +. (indice m 2 2)*.c_22 +. (indice m 3 2)*.c_32
  ;;
