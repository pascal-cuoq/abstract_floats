all: build

build: float.ml test.ml
	ocamlc -g -c float.ml
	ocamlc -g -c test.ml
	ocamlc -g float.cmo test.cmo

clean:
	rm -f *.cmo *.cmi *.cmp *.out
