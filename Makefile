# Makefile pour le projet OCaml

# Compilateur OCaml
OCAMLC=ocamlfind ocamlc

# Options pour le compilateur
OCAMLFLAGS=-linkpkg -package zarith

# Noms des exécutables
EXECUTABLES=encode decode

all: $(EXECUTABLES)

encode: encode.ml
	$(OCAMLC) $(OCAMLFLAGS) -o encode encode.ml

decode: decode.ml
	$(OCAMLC) $(OCAMLFLAGS) -o decode decode.ml

clean:
	rm -f *.cmi *.cmo $(EXECUTABLES)

.PHONY: all clean
