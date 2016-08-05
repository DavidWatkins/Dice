TARGET=src/dice
LIBS=-I,/usr/lib/ocaml/
FLAGS= -j 0 -r -use-ocamlfind -pkgs yojson,llvm,llvm.analysis,llvm.bitwriter,llvm.bitreader,llvm.linker,llvm.target,batteries
OCAMLBUILD=ocamlbuild
OPAM=opam config env
CLIBEXT=_includes

.PHONY: master.pdf all clean pdf pdfclean

all: native pdf
	@clang-3.7 -c -emit-llvm src/bindings.c
	@mkdir -p $(CLIBEXT)
	@mv bindings.bc $(CLIBEXT)/bindings.bc
	@cp src/stdlib.dice $(CLIBEXT)/stdlib.dice
	@mv dice.native dice
	@echo Compilation Complete

clean: pdfclean
	@cd src
	$(OCAMLBUILD) -clean
	@cd ..
	@rm -rf $(CLIBEXT)
	@echo cleaning complete

pdfclean:
	@cd report;latexmk -CA;rm -rf ./_minted-master;rm -f ./Code/code.tex;rm -f ./Code/tests.tex;rm -f ./Code/demo.tex;rm -f ./Includes/gitlog.tex

native:
	@cd src
	@eval `opam config env`
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native
	@cd ..

byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte

pdf: master.pdf

master.pdf: 
	@cd report; ls; python make_tex.py; pdflatex -synctex=1 -interaction=nonstopmode "master".tex -shell-escape; mv master.pdf ../dice.pdf
	@echo dice.pdf created

depend:
	echo "Not needed." 