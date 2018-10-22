#!/bin/bash

echo Compilation en cours

ocamlc -c -I Compilables -o Compilables/alphaset.cmi Compilables/alphaset.mli
ocamlc -c -I Compilables -o Compilables/alphaset.cmo common/alphaset.ml

ocamlc -c -I Compilables -o Compilables/pointtriangle.cmi Compilables/pointtriangle.mli
ocamlc -c -I Compilables -o Compilables/pointtriangle.cmo common/pointtriangle.ml

ocamlc -c -I Compilables -o Compilables/matrix.cmi Compilables/matrix.mli
ocamlc -c -I Compilables -o Compilables/matrix.cmo Detection/matrix.ml

ocamlc -c -I Compilables -o Compilables/detec.cmi Compilables/detec.mli
ocamlc -c -I Compilables -o Compilables/detec.cmo Detection/detec.ml

ocamlc -c -I Compilables -o Compilables/changement.cmi Compilables/changement.mli
ocamlc -c -I Compilables -o Compilables/changement.cmo Change/changement.ml

ocamlc -c -I Compilables -o Compilables/stepbystepmorphism.cmi Compilables/stepbystepmorphism.mli
ocamlc -c -I Compilables -o Compilables/stepbystepmorphism.cmo Morphism/stepbystepmorphism.ml

ocamlc -c -I Compilables -o Compilables/display.cmi Compilables/display.mli
ocamlc -c -I Compilables -o Compilables/display.cmo Graphic/display.ml

ocamlc -c -I Compilables -o Compilables/delaunay.cmi Compilables/delaunay.mli
ocamlc -c -I Compilables -o Compilables/delaunay.cmo common/delaunay.ml

ocamlc -c -I Compilables -o Compilables/affichagemorph.cmi Compilables/affichagemorph.mli
ocamlc -c -I Compilables -o Compilables/affichagemorph.cmo Graphic/affichagemorph.ml

ocamlc -c -I Compilables -o Compilables/mouse_test.cmi Compilables/mouse_test.mli
ocamlc -c -I Compilables -o Compilables/mouse_test.cmo Graphic/mouse_test.ml


ocamlc -a Compilables/pointtriangle.cmo Compilables/matrix.cmo Compilables/alphaset.cmo Compilables/detec.cmo -o Compilables/tools.cma
ocamlc -a Compilables/display.cmo  Compilables/changement.cmo -o Compilables/uppertools.cma

ocaml Compilables/main.ml
