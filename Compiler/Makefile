#!/bin/sh

TARGET=dice
FLAGS=-libs unix,nums
OCAMLBUILD=ocamlbuild

all: native
	cp -L _build/$(TARGET).native $(TARGET)

clean:
	$(OCAMLBUILD) -clean
	rm $(TARGET)

native:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native
byte:
	$(OCAMLBUILD) $(FLAGS) $(TARGET).byte
depend:
	echo "Not needed." 

