#!/bin/bash

echo Detection
echo detec.ml ../common/Interfaces/detec.mli test.ml :
rm ./*.cm*
ocamlc -c -o detec.cmi ../common/Interfaces/detec.mli
ocamlc -c -o detec.cmo detec.ml
ocamlc -a -o detec.cma detec.cmo
ocaml test.ml
