#!/bin/bash

# Edit this for your own project dependencies
OPAM_DEPENDS="core batteries llvm=3.8 yojson ocamlfind"

APT="ocaml ocaml-native-compilers camlp4-extra opam m4 clang-3.8 llvm"
	 	 
echo "yes" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq $APT
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
make test