.DEFAULT_GOAL := vlcc

# main compilation
OBJS = parser.cmo scanner.cmo vlcc.cmo
vlcc : $(OBJS)
	ocamlc -o vlcc $(OBJS)

# Rule to build only scanner
scanner_test : scanner.cmo
	ocamlc -o scanner_test scanner.cmo

# Intermediate rules
# Lexxer rules
%.ml : %.mll
	ocamllex $<

# Parser rules
%.ml %.mli : %.mly
	ocamlyacc $<

# OCaml Object file rules
%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

# Alias for cleaning up code
.PHONY : clean
clean :
	rm -f vlcc parser.ml parser.mli scanner.ml *.cmo *.cmi
