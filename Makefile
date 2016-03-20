all: build

build: float.ml test.ml
	ocamlc -c float.ml
	ocamlc -c test.ml
	ocamlc float.cmo test.cmo

clean:
	rm -f *.cmo *.cmi *.cmp *.out
