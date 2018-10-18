type 'a set = 'a list;;

let empty() = [];;

let cons l x = x::l;;

let car l = List.hd l;;

let cdr l = List.tl l;;

let concat l1 l2 = l1@l2;;

let length l = List.length l;;

let rec find l x = match l with
    | [] -> false
    | h::_ when h = x -> true
    | _::t -> find t x
;;

let rec suppress refl x =
    let rec suppress_aux l = match l with
        | [] -> []
        | h::t when h = x -> suppress_aux t
        | h::t -> h :: (suppress_aux t)
    in refl := suppress_aux (!refl);;

let rec iter f l = match l with
    | [] -> ()
    | h::t -> (f(h) ; iter f t)
;;

let is_empty l = l = [];;

let copy l = l;;

let rec sort ord l =
    if is_empty l then empty()
    else
        let l_inf = ref (empty()) in
        let l_sup = ref (empty()) in
        let f_point = car l in
        let aux_sort curr_point =
            if ord curr_point f_point then
                l_inf := cons (!l_inf) curr_point
            else
                l_sup := cons (!l_sup) curr_point
        in iter aux_sort (cdr l);
    (sort ord !l_inf)@[f_point]@(sort ord !l_sup);;
