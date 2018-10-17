type 'a matrix = 'a list list;;

let index l i =
  let rec aux l i k = match l with
    |[] -> failwith "index"
    |t::q -> if i = k then t else aux l i (k+1);

    in aux l i 0;;

let indice m i j = index (index m i) j;;

let det_2 m =
    (indice m 1 1) *.(indice m 2 2) -. (indice m 1 2) *.(indice m 2 1)
    ;;

let det_3 m =
  let c_12 = in
  let c_22 = in
  let c_32 = in
  (indice m 1 2)*.c_12 +. (indice m 2 2)*.c_22 +. (indice m 3 2)*.c_32
  ;;
