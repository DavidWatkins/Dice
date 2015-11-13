OBJS = unix.cma utils.cmo exceptions.cmo scanner.cmo parser.cmo ast.cmo processor.cmo dice.cmo

dice : $(OBJS)
	ocamlc -o dice $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc -v parser.mly

exceptions.cmo :
exceptions.cmx :
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmo parser.cmi
dice.cmo : utils.cmo processor.cmo parser.cmi exceptions.cmo
dice.cmx : utils.cmx processor.cmx parser.cmx exceptions.cmx
processor.cmo : scanner.cmo parser.cmi
processor.cmx : scanner.cmx parser.cmx
scanner.cmo : parser.cmi exceptions.cmo
scanner.cmx : parser.cmx exceptions.cmx
utils.cmo : processor.cmo parser.cmi ast.cmo
utils.cmx : processor.cmx parser.cmx ast.cmo
parser.cmi : ast.cmo
unix.cma : 

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f dice parser.ml parser.mli scanner.ml \
	*.cmo *.cmi *.out *.diff *.orig *.output
