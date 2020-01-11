OC = ocamlc
OCFLAGS =
OCCMD = $(OC) $(OCFLAGS)

BrainfInterpreter.cmi:
	$(OCCMD) -c BrainfInterpreter.mli

bfi: BrainfInterpreter.cmi
	$(OCCMD) -o bfi BrainfInterpreter.ml App.ml

all: bfi

clean:
	rm -rf *.annot *.cmo *.cma *.cmi *.a *.o *.cmx *.cmxs *.cmxa *.out bfi
