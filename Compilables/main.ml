#load "graphics.cma";;
#load "tools.cma";;
#load "uppertools.cma";;


open Unix;;
open Graphics;;
open Pervasives;;
open Alphaset;;
open Pointtriangle;;
open Matrix;;
open Detec;;
open Changement;;
open Stepbystepmorphism;;
open Display;;
open Dynamic_display;;
open Delaunay;;


Random.self_init();;

init_display 800 600;;

let p_set = rand_points 100 800. 600.;;
let t_set = delaunay p_set 800 600;;

draw_triangle t_set;;
draw_point p_set;;

let _ = Graphics.wait_next_event [Key_pressed] in ();;

init_display 800 600 ;;
delaunay_stepwise (rand_points 20 800. 600.) 800 600;;

print_string "Appuyer sur la touche Entrée pour continuer";;

let _ = Graphics.wait_next_event [Key_pressed] in ();;

print_string "Appuyer sur + pour ajouter un point. \n";;
print_string "Appuyer sur - pour supprimer un point. \n";;
print_string "Cliquer et déplacer pour déplacer un point. \n";;
print_string "Appuyer sur q pour quitter.";;

init_display 800 600;;
try
    run (rand_points 30 800. 600.);
with
  |_ -> ();;

print_string "Appuyer sur la touche Entrée pour continuer";;
let _ = Graphics.wait_next_event [Key_pressed] in ();;

init_display 800 600 ;;
try
    animated_morphprint();
with
| _ -> ();;

let _ = Graphics.wait_next_event [Key_pressed] in ();;
