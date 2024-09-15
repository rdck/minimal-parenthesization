precedence: STLC.ml Test.ml
	ocamlfind ocamlopt -o precedence -linkpkg -package core STLC.ml Test.ml

.PHONY: clean

clean:
	rm -f precedence *.cmi *.cmx *.o
