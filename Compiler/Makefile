TARGET=dice
LIBS=-I,/usr/lib/ocaml/
FLAGS= -j 0 -use-ocamlfind -pkgs yojson,llvm,llvm.analysis,llvm.bitwriter,llvm.bitreader,llvm.linker,llvm.target,batteries
OCAMLBUILD=ocamlbuild
OPAM=opam config env
CLIBEXT=c_lib_ext

all: clean native
	mv dice.native dice
	clang-3.7 -c -emit-llvm bindings.c
	mkdir $(CLIBEXT)
	mv bindings.bc $(CLIBEXT)/bindings.bc

clean:
	$(OCAMLBUILD) -clean
	rm -rf $(CLIBEXT)

native:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native

byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte
depend:
	echo "Not needed." 