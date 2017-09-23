#!/bin/bash

# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind llvm3.8"

APT="ocaml ocaml-native-compilers camlp4-extra opam m4 clang-3.8 llvm opam ocaml"
	 
ppa=avsm/ppa
	 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq $APT
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
make test