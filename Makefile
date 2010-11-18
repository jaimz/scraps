# Write here all the findlib packages you need, for example:
# PACKAGES= ,extlib,netstring
PACKAGES= ,cryptokit,json-wheel,type-conv,uuidm,str

# Write here all your .ml files, in dependency order (default: all)
# example: 
# FILES= mod1.ml mod2.ml mod3.ml

FILES= couchdb.ml note.ml account.ml services.ml


CAMLC = ocamlfind ocamlc -g -thread $(LIB)
CAMLOPT = ocamlfind ocamlopt -thread $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
OCSIGENREP = `ocamlfind query ocsigen`
TYPE_CONV_DIR = `ocamlfind query type-conv`
JSONTCDIR = `ocamlfind query json-tc`
#OCSIGENREP = ../ocsigen/lib
LIB = -package lwt,ocsigen$(PACKAGES) -I $(OCSIGENREP)
# If you use the syntax extension:
#PP = -pp "camlp4o $(OCSIGENREP)/xhtmlsyntax.cma"
PP = -pp "camlp4o $(TYPE_CONV_DIR)/pa_type_conv.cmo $(JSONTCDIR)/pa_json_tc.cmo"
# otherwise
# PP = 

OBJS = $(FILES:.ml=.cmo)

CMA = services.cma

all: depend $(CMA) install

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) $(OBJS)

install:
	chmod a+r $(CMA)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
#	$(CAMLDOC) -d doc -html db.mli

clean:
	-rm -f *.cm[ioxa] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend


  