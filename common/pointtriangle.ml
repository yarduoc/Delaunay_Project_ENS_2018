
#use "common/alphaset.ml"

type point = {x: float; y: float};;
type morph_point = {mx: float; my: float; label: int};;
type triangle = { p1 : point; p2 : point ; p3 : point};;

type point_set = point list;;
type morph_point_set = morph_point set;;

type triangle_set = triangle list;;

let make_triangle a b c = {p1 = a; p2 = b; p3 = c};;
let make_point a b = {x = a; y = b};;
let make_morph_point a b l = {mx = a; my = b; label = l};;

let point_to_morph_point point label =
    let new_x = int_of_float point.x in
    let new_y = int_of_float point.y in
    make_morph_point new_x new_y label
;;

let morph_point_to_point m_point =
    let new_x = float_of_int m_point.mx in
    let new_y = float_of_int m_point.my in
    make_point new_x new_y
;;

let ord_points p1 p2 =
    if p1.x = p2.x then
        p1.y < p2.y
    else p1.x < p2.x
;;

let ord_m_points p1 p2 = p1.label < p2.label;;

let print_point point =
    print_string "(";
    print_float point.x;
    print_string ";";
    print_float point.y;
    print_string ")"
;;

let print_triangle t =
    print_string "Triangle : p1 = ";
    print_point t.p1;
    print_string " p2 = ";
    print_point t.p2;
    print_string " p3 = ";
    print_point t.p3;
    print_newline ()
;;

let print_triangle_set t_set = iter print_triangle t_set;;
