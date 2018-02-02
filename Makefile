TARGETS = snick
TARGETS_BYTE=$(TARGETS:%=%.byte)

MODULES = snick_ast snick_lex snick_parse symbol analyze type_checking codegen
MLFILES = $(addsuffix .ml, $(MODULES))
CMOFILES = $(addsuffix .cmo, $(MODULES))
CMXFILES = $(addsuffix .cmx, $(MODULES))

ALLMODULES = $(MODULES) snick

OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLDEP = ocamldep

OCAMLFLAGS =

all : opt byte
byte: $(TARGETS_BYTE)
opt: $(TARGETS)

%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -c $<

%.cmo: %.ml
	ocamlc $(OCAMLFLAGS) -g -c $<

%.cmx: %.ml
	ocamlopt $(OCAMLOPTFLAGS) -g -c $<

%.ml: %.mll
	$(OCAMLLEX) $^

%.ml %.mli: %.mly
	$(OCAMLYACC) $^

snick.byte : $(CMOFILES) snick.cmo
	ocamlc -g -o $@ $^

snick : $(CMXFILES) snick.cmx
	ocamlopt -g -o $@ $^
	submit COMP90045 3b.late Makefile Makefile.depend snick_lex.mll snick_parse.mly snick_ast.ml symbol.ml type_checking.ml analyze.ml codegen.ml snick.ml

clean :
	rm -f *.cmo *.cmi *.cmx *.o
	rm -f snick_lex.ml snick_parse.ml snick_parse.mli

clobber : clean
	rm -f $(TARGETS) $(TARGETS_BYTE)

.PHONY : clean clobber depend

# include depend
depend: snick_lex.ml snick_parse.ml analyze.ml
	$(OCAMLDEP) snick.ml snick.mli $(ALLMODULES:%=%.mli) $(ALLMODULES:%=%.ml) >Makefile.depend

-include Makefile.depend
