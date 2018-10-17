(*Builders and modifiers*)
let cr_m33 () = Array.make_matrix 3 3 0. ;;

(*Getters*)

let indice m i j = m.(1).(j);; (* give M[i,j]*)

let rec shape m = Array.length  m

(*Calculus methods*)

let det_2 m = (*determinant 2*2 *)
    (indice m 0 0) *.(indice m 1 1) -. (indice m 0 1) *.(indice m 1 0)
    ;;

let mineur m i j = (*give the minor of m develop with M[i,j]*)
  let  minor = Array.make_matrix 2 2 0. in
  let a = ref 0 and b = ref 0 in
  for k=0 to 2 do
    for l=0 to 2 do
      if (k !=i && l != j && !a < 2 )
       then begin minor.(!b).(!a) <-  m.(k).(l);
          a:= 1 + (!a); print_int (!a)
            end;
      done;
      if (k !=i && !b < 2)
        then  begin
              b := 1+ (!b);
              a:= 0
              end;

    done;
    print_string "det";
    det_2 minor;;



let det_3 m = (*determinant 3*3 *)
  let c_12 = -.(mineur m 0 1)in
  let c_22 = (mineur m 1 1)in
  let c_32 = -. (mineur m 2 1)in
  (indice m 1 2)*.c_12 +. (indice m 2 2)*.c_22 +. (indice m 3 2)*.c_32
  ;;
