(*Builders and modifiers*)
let create_matrix_3x3 () = Array.make_matrix 3 3 0. ;;

(*Getters*)

let indice m i j = m.(i).(j);;

(*Calculus methods*)

let det_2 m =
    (indice m 0 0) *. (indice m 1 1) -. (indice m 0 1) *. (indice m 1 0)
;;

(* Change a float array of size n in a float array of size n-1 removing the index i*)
let reduction_i line i =
    let curr_index = ref 0 in
    let size = Array.length line in
    let new_array = Array.make (size-1) 0. in
    for k = 0 to (size - 1) do
        if k != i then
            begin
            new_array.(!curr_index) <- line.(k);
            curr_index := !curr_index + 1;
            end
    done;
    new_array
;;

let get_minor matrix i j =
    let size = (Array.length matrix) - 1 in
    let minor_matrix = Array.make size [|0.|] in
    let is_ahead = ref false in
    for index = 0 to size do
        if index != i && !is_ahead then
            minor_matrix.(index - 1) <- reduction_i matrix.(index) j
        else if index != i then
            minor_matrix.(index) <- reduction_i matrix.(index) j
        else
            is_ahead := true
    done;
    minor_matrix
;;

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


let det_3 matrix = (*determinant 3*3 *)
  let c_01 = -.det_2 (get_minor matrix 0 1)in
  let c_11 =   det_2 (get_minor matrix 1 1)in
  let c_21 = -.det_2 (get_minor matrix 2 1)in
  (indice matrix 0 1)*.c_01 +. (indice matrix 1 1)*.c_11 +. (indice matrix 2 1)*.c_21
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
