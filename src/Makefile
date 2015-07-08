.DEFAULT_GOAL := vlcc

   OCAMLC=ocamlc   
 OCAMLOPT=ocamlopt 
 OCAMLDEP=ocamldep 
 OCAMLLEX=ocamllex 
OCAMLYACC=ocamlyacc
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

.SECONDARY:
	

# main compilation
MAIN_OBJS = scanner.cmo parser.cmo ast.cmi vlcc.cmo
vlcc : $(MAIN_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $<

# create test binaries
SCANNER_OBJS = scanner.cmo ast.cmi scanner_test.cmo
scanner_test : $(SCANNER_OBJS)
	$(OCAMLC) -o $@ $<

PARSER_OBJS = scanner.cmo parser.cmo ast.cmi parser_test.cmo
parser_test : $(PARSER_OBJS)
	$(OCAMLC) -o $@ $<

# Lexxer rules
%.ml : %.mll
	$(OCAMLLEX)   $<

# Parser rules
%.ml %.mli : %.mly
	$(OCAMLYACC)    $<

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

# Clean up
clean:
	rm -f vlcc
	rm -f scanner_test parser_test
	rm -f scanner.ml parser.ml parser.mli
	rm -f *.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
