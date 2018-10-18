#!/bin/bash

echo Compilation en cours
rm -r *.cm*

ocamlc -c -I Compilables -o Compilables/pointtriangle.cmi Compilables/pointtriangle.mli
ocamlc -c -I Compilables -o Compilables/pointtriangle.cmo common/pointtriangle.ml


echo pointtriangle OK

ocamlc -c -I Compilables -o Compilables/matrix.cmi Compilables/matrix.mli
ocamlc -c -I Compilables -o Compilables/matrix.cmo Detection/matrix.ml


echo matrix OK

ocamlc -c -I Compilables -o Compilables/alphaset.cmi Compilables/alphaset.mli
ocamlc -c -I Compilables -o Compilables/alphaset.cmo common/alphaset.ml


echo alphaset OK

ocamlc -c -I Compilables -o Compilables/detec.cmi Compilables/detec.mli
ocamlc -c -I Compilables -o Compilables/detec.cmo Detection/detec.ml

echo detec OK

ocamlc -c -I Compilables -o Compilables/display.cmi Compilables/display.mli
ocamlc -c -I Compilables -o Compilables/display.cmo Graphic/display.ml

echo display OK


ocamlc -c -I Compilables -o Compilables/changement.cmi Compilables/changement.mli
ocamlc -c -I Compilables -o Compilables/changement.cmo Change/changement.ml

echo changement OK

ocamlc -a Compilables/pointtriangle.cmo  Compilables/matrix.cmo Compilables/alphaset.cmo Compilables/display.cmo Compilables/detec.cmo Compilables/changement.cmo -o common/BastideCoudrayDesauw.cma

ocaml common/main.ml
