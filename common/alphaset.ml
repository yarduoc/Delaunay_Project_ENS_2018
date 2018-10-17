type 'a set = 'a list;;

let empty() = [];;

let cons l x = x::l;;
let car l = List.hd l;;
let cdr l = List.tl l;;

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
