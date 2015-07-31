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
MAIN_OBJS = xst.cmo errors.cmo xscanner.cmo xparser.cmo \
			blockify.cmo bytecode.cmo compile.cmo optimize.cmo \
			vislang.cmo
vlcc : $(MAIN_OBJS)
	@echo "$(OCAMLC) -o vlcc"
	@$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

# Lexxer rules
%.ml : %.mll
	$(OCAMLLEX) -q $<

# Parser rules
%.ml %.mli : %.mly
	$(OCAMLYACC)    $<

# Rule to make interface files
%.mli: %.ml
	$(OCAMLC) -i $< > $@

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
	@echo "Cleaning Directory..."
	@rm -f vlcc
	@rm -f xscanner.ml xparser.ml xparser.mli
	@rm -f bot.mli blockify.mli
	@rm -f *.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
