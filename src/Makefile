.DEFAULT_GOAL := vlcc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

# main compilation
MAIN_OBJS = parser.cmo scanner.cmo ast.cmi vlcc.cmo
vlcc : $(MAIN_OBJS)
	$(OCAMLC) -o vlcc $(OCAMLFLAGS) $(MAIN_OBJS)

# Lexxer rules
%.ml : %.mll
	$(OCAMLLEX) $<

# Parser rules
%.ml %.mli : %.mly
	$(OCAMLYACC) $<

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f vlcc
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
