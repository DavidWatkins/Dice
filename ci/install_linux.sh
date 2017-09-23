#!/bin/bash
APT='
opam
m4
pkg-config
make
llvm-3.8
llvm
'

sudo apt-get -qq --yes --force-yes install $APT
opam init -y
eval $(opam config env)
opam install ocamlfind -y
opam install llvm.3.8 -y
export PATH=/usr/lib/llvm-3.8/bin:$PATH