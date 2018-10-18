#!/bin/bash

echo Compilation en cours
rm -r *.cm*

ocamlc -I Compilables/ -c -o Compilables/alphaset.cmi common/Interfaces/alphaset.mli
ocamlc -I Compilables/ -c -o Compilables/alphaset.cmo common/alphaset.ml
ocamlc -I Compilables/ -a -o alphaset.cma Compilables/alphaset.cmo

echo Alphaset OK

ocamlc -I Compilables/ -c -o Compilables/pointtriangle.cmi common/Interfaces/pointtriangle.mli
ocamlc -I Compilables/ -c -o Compilables/pointtriangle.cmo common/pointtriangle.ml
ocamlc -I Compilables/ -a -o Compilables/pointtriangle.cma Compilables/pointtriangle.cmo Compilables/alphaset.cmo

echo Pointtriangle OK

ocamlc -I Compilables/ -c -o Compilables/matrix.cmi common/Interfaces/matrix.mli
ocamlc -I Compilables/ -c -o Compilables/matrix.cmo Detection/matrix.ml
ocamlc -I Compilables/ -a -o Compilables/matrix.cma Compilables/matrix.cmo

echo Matrix OK

ocamlc -I Compilables/ -c -o Compilables/detec.cmi common/Interfaces/detec.mli
ocamlc -I Compilables/ -c -o Compilables/detec.cmo Detection/detec.ml
ocamlc -I Compilables/ -a -o Compilables/detec.cma Compilables/detec.cmo Compilables/pointtriangle.cmo Compilables/alphaset.cmo Compilables/matrix.cmo

echo Detec OK

ocamlc -I Compilables/ -c -o Compilables/display.cmi common/Interfaces/display.mli
ocamlc -I Compilables/ -c -o Compilables/display.cmo Graphic/display.ml
ocamlc -I Compilables/ -a -o Compilables/display.cma Compilables/display.cmo Compilables/detec.cmo Compilables/pointtriangle.cmo Compilables/alphaset.cmo Compilables/matrix.cmo

echo Display OK

ocamlc -I Compilables/ -c -o Compilables/changement.cmi common/Interfaces/changement.mli
ocamlc -I Compilables/ -c -o Compilables/changement.cmo Change/changement.ml
ocamlc -I Compilables/ -a -o Compilables/changement.cma Compilables/changement.cmo Compilables/detec.cmo Compilables/pointtriangle.cmo Compilables/alphaset.cmo Compilables/matrix.cmo

echo Changements OK

echo Compilation terminée
echo "\n"

read x

ocaml Compilables/main.ml
