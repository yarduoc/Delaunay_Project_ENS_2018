#!/bin/bash

echo Compilation en cours
rm -r *.cm*
r
ocamlc -c -o Compilables/point_triangle.cmi common/Interfaces/point_triangle.mli
ocamlc -c -o Compilables/point_triangle.cmo common/point_triangle.ml
ocamlc -a -o point_triangle.cma Compilables/point_triangle.cmo

ocamlc -c -o Compilables/matrix.cmi common/Interfaces/matrix.mli
ocamlc -c -o Compilables/matrix.cmo Detection/matrix.ml
ocamlc -a -o matrix.cma Compilables/matrix.cmo

ocamlc -c -o Compilables/alphaset.cmi common/Interfaces/alphaset.mli
ocamlc -c -o Compilables/alphaset.cmo common/alphaset.ml
ocamlc -a -o alphaset.cma Compilables/alphaset.cmo

ocamlc -c -o Compilables/display.cmi common/Interfaces/display.mli
ocamlc -c -o Compilables/display.cmo Graphic/display.ml
ocamlc -a -o display.cma Compilables/display.cmo

ocamlc -c -o Compilables/detec.cmi common/Interfaces/detec.mli
ocamlc -c -o Compilables/detec.cmo Detection/detec.ml
ocamlc -a -o detec.cma Compilables/detec.cmo

ocamlc -c -o Compilables/changement.cmi common/Interfaces/changement.mli
ocamlc -c -o Compilables/changement.cmo Change/changement.ml
ocamlc -a -o changement.cma Compilables/changement.cmo

echo Compilation terminée

ocaml main.ml
