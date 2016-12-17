# Edit this for your own project dependencies
OPAM_DEPENDS="core batteries llvm.3.8 yojson ocamlfind"
	 
case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac
	 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra
sudo apt-get install m4 clang-3.8 llvm opam ocaml
export OPAMYES=1
opam init 
opam depext conf-pkg-config.1.0
opam install ${OPAM_DEPENDS}
opam switch 4.02.1
eval `opam config env`
make
make test