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
open Dynamicdisplay;;
open Delaunay;;


Random.self_init();;

init_display 1000 800;;

let p_set = rand_points 100 1000. 800.;;
let t_set = delaunay p_set 1000 800;;

draw_triangle t_set;;
draw_point p_set;;

print_string "Appuyer sur la touche Entrée pour continuer";;
read_line();;


init_display 1000 800 ;;
delaunay_stepwise (rand_points 20 1000. 800.) 1000 800;;

print_string "Appuyer sur la touche Entrée pour continuer";;
read_line();;
print_string "Appuyer sur + pour ajouter un point. \n";;
print_string "Appuyer sur - pour supprimer un point. \n";;
print_string "Cliquer et déplacer pour déplacer un point. \n";;
print_string "Appuyer sur q pour quitter.";;
read_line();;

init_display 1000 800;;
try
    run (rand_points 30 1000. 800.);
with _ -> ();;

print_string "Appuyer sur la touche Entrée pour continuer";;
read_line();;
init_display 1000 800 ;;
