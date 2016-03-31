all: build

build: float.ml test.ml dichotomy.ml dichotomy_test.ml
	ocamlc -g -c float.ml
	ocamlc -g -c test.ml
	ocamlc -g -c dichotomy.ml
	ocamlc -g -c dichotomy_test.ml
	ocamlc -g float.cmo test.cmo
	ocamlc -g dichotomy.cmo dichotomy_test.cmo -o dich_test.out

coverage:
	bisect-ppx-report -I bulid/ -html . bisect*.out

clean:
	rm -rf coverage
	rm -f *.cmo *.cmi *.cmp *.out *.html
