(*Builders and modifiers*)
let cr_m33 () = Array.make_matrix 3 3 0. ;;

(*Getters*)

let indice m i j = m.(i).(j);; (* give M[i,j]*)

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
          a:= 1 + (!a)
            end;
      done;
      if (k !=i && !b < 2)
        then  begin
              b := 1+ (!b);
              a:= 0
              end;

    done;
    det_2 minor;;


let mineur_3 m i j = (*give the minor of m develop with M[i,j]*)
      let  minor = Array.make_matrix 2 2 0. in
      let a = ref 0 and b = ref 0 in
      for k=0 to 2 do
        for l=0 to 2 do
          if (k !=i && l != j && !a < 2 )
           then begin minor.(!b).(!a) <-  m.(k).(l);
              a:= 1 + (!a)
                end;
          done;
          if (k !=i && !b < 2)
            then  begin
                  b := 1+ (!b);
                  a:= 0
                  end;

        done;
        det_3 minor;;


let det_3 m = (*determinant 3*3 *)  (*is correct*)
  let c_01 = -.(mineur m 0 1)in
  let c_11 = (mineur m 1 1)in
  let c_21 = -.(mineur m 2 1)in
  (indice m 0 1)*.c_01 +. (indice m 1 1)*.c_11 +. (indice m 2 1)*.c_21
  ;;

let rec puissance a n = if n= 0 then 1
    else a*.(puissance a (n-1));;

let det_4 m =
  let s= ref 0 and j = 4 in
    for i=0 to 3
      do s:= (puissance (-1.) (i+j)) *. (mineur_3 m i j) +. (!s) done

  !s;;

let m = [| [|1.;2.;3.;4.|];
            [|1.;2.;3.;-.4.|];
            [|-.1.;2.;3.;4.|];
            [|1.;-.2.;3.;4.|] |];;

det_4 m;;
