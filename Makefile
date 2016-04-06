all: build

build: float.ml test.ml dichotomy.ml dichotomy_test.ml
	ocamlopt -unsafe -g -c float.ml
	ocamlopt -unsafe -g -c test.ml
	ocamlopt -unsafe -g -c dichotomy.ml
	ocamlopt -unsafe -g -c dichotomy_test.ml
	ocamlopt -unsafe -g float.cmx test.cmx -o test.out
	ocamlopt -unsafe -g dichotomy.cmx dichotomy_test.cmx -o dich_test.out

coverage:
	bisect-ppx-report -I bulid/ -html . bisect*.out

clean:
	rm -rf coverage
	rm -f *.cmo *.cmi *.cmp *.out *.html *.o
