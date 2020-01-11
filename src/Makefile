OC = ocamlc
OCFLAGS =
OCCMD = $(OC) $(OCFLAGS)

BrainfInterpreter.cmi:
	$(OCCMD) -c BrainfInterpreter.mli

BrainfInterpreter.cmo: BrainfInterpreter.cmi
	$(OCCMD) -c BrainfInterpreter.ml

bfi: BrainfInterpreter.cmi BrainfInterpreter.cmo
	$(OCCMD) -o bfi App.ml

all: bfi

clean:
	rm -rf *.annot *.cmo *.cma *.cmi *.a *.o *.cmx *.cmxs *.cmxa *.out bfi
