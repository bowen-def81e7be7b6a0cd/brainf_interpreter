OC = ocamlc
OCFLAGS =
OCCMD = $(OC) $(OCFLAGS)

brainf_interpreter.cmi:
	$(OCCMD) -c brainf_interpreter.mli

brainf_interpreter.cmo: brainf_interpreter.cmi
	$(OCCMD) -c brainf_interpreter.ml

bfi: brainf_interpreter.cmi brainf_interpreter.cmo
	$(OCCMD) -open brainf_interpreter.cmo -o bfi app.ml

all: bfi

clean:
	rm -rf *.annot *.cmo *.cma *.cmi *.a *.o *.cmx *.cmxs *.cmxa *.out bfi
