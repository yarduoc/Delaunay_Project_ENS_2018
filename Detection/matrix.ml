type 'a matrix = 'a list list;;

let cr_m33 () =
  let  matrice = ref [] in
  for k=0 to shape m

    do let line= ref [] in
    for l=0 to shape m

      do if (k !=i && l != j)
        begin  line := 0::(!line)
        end;
      done;
      matrice := (!line)::(!matrice)
    done;
    (!matrice);;

let replace_list l i x =
  let rec aux l i x k =
    match l with
      |[] -> failwith "replace"
      |t::q -> if k = i then x::q
                else t::(aux l i x (k+1))
  in aux l i x 0;;

let change matrice i j x =
  replace_list matrice i (replace_list (index matrice i) j x);;

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
  let  mineur_m = ref [] in
  for k=0 to shape m

    do let mini = ref [] in
    for l=0 to shape m

      do if (k !=i && l != j)
        begin  mini := (indice m k l)::(!mini)
        end;
      done;
      mineur_m := (!mini)::(!mineur_m)
    done;
    (!mineur);;


let det_2 m = (*determinant 2*2 *)
    (indice m 1 1) *.(indice m 2 2) -. (indice m 1 2) *.(indice m 2 1)
    ;;

let det_3 m = (*determinant 3*3 *)
  let c_12 = -(mineur m 1 2)in
  let c_22 = (mineur m 2 2)in
  let c_32 = - (mineur m 3 2)in
  (indice m 1 2)*.c_12 +. (indice m 2 2)*.c_22 +. (indice m 3 2)*.c_32
  ;;
