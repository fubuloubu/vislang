.DEFAULT_GOAL := vlcc

   OCAMLC=ocamlc   
 OCAMLOPT=ocamlopt 
 OCAMLDEP=ocamldep 
 OCAMLLEX=ocamllex 
OCAMLYACC=ocamlyacc

# main compilation
.SECONDARY:
MAIN_OBJS = xst.cmo errors.cmo xscanner.cmo xparser.cmo \
			blockify.cmo blockparse.cmo compile.cmo vislang.cmo
vlcc : $(MAIN_OBJS)
	@echo "$(OCAMLC) -o vlcc"
	@$(OCAMLC) -o $@ str.cma $^

# Lexxer rules
%.ml : %.mll
	$(OCAMLLEX) -q $<

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

clean:
	rm -f vlcc
	rm -f xscanner.ml xparser.ml xparser.mli
	rm -f *.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
