TARGET=src/dice
LIBS=-I,/usr/lib/ocaml/
FLAGS= -j 0 -r -use-ocamlfind -pkgs yojson,llvm,llvm.analysis,llvm.bitwriter,llvm.bitreader,llvm.linker,llvm.target,batteries
OCAMLBUILD=ocamlbuild
OPAM=opam config env
CLIBEXT=includes


all: native
	@clang-3.7 -c -emit-llvm src/bindings.c
	@mkdir -p $(CLIBEXT)
	@mv bindings.bc $(CLIBEXT)/bindings.bc
	@mv dice.native dice
	@echo Compilation Complete

clean:
	@cd src
	$(OCAMLBUILD) -clean
	@cd ..
	@rm -rf $(CLIBEXT)
	@rm dice
	@echo Cleaning Complete

native:
	@cd src
	@eval `opam config env`
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native
	@cd ..

byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte

depend:
	echo "Not needed." 