TARGET=dice
LIBS=-I,/usr/lib/ocaml/
FLAGS=-pkg llvm
OCAMLBUILD=ocamlbuild

all: native
	mv dice.native dice

clean:
	$(OCAMLBUILD) -clean
	rm $(TARGET)

native:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native
byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte
depend:
	echo "Not needed." 

