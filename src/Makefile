OC = ocamlc
OCFLAGS =
OCCMD = $(OC) $(OCFLAGS)

brainf_parser.cmi:
	$(OCCMD) -c brainf_parser.mli

brainf_parser.cmo: brainf_parser.cmi
	$(OCCMD) -c brainf_parser.ml

brainf_interpreter.cmi:
	$(OCCMD) -c brainf_interpreter.mli

brainf_interpreter.cmo: brainf_interpreter.cmi brainf_parser.cmo
	$(OCCMD) -c brainf_interpreter.ml

bfi: brainf_interpreter.cmi brainf_interpreter.cmo
	$(OCCMD) -o bfi app.ml

all: bfi

clean:
	rm -rf *.annot *.cmo *.cma *.cmi *.a *.o *.cmx *.cmxs *.cmxa *.out bfi
