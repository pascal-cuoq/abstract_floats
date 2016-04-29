all: build

build: c_bindings.c float.ml test.ml dichotomy.ml dichotomy_test.ml
	ocamlfind ocamlopt -package num -c -g -unsafe c_bindings.c float.ml
	ocamlfind ocamlopt -c -g -unsafe test.ml
	ocamlfind ocamlopt -linkpkg -g -unsafe -package num c_bindings.o float.cmx test.cmx -o test.out
	ocamlfind ocamlopt -package num -c -g -unsafe dichotomy.ml
	ocamlfind ocamlopt -c -g -unsafe dichotomy_test.ml
	ocamlfind ocamlopt -linkpkg -g -unsafe -package num dichotomy.cmx dichotomy_test.cmx -o test_dich.out

clean:
	rm -rf coverage
	rm -f *.cmo *.cmi *.cmx *.cmp *.out *.html *.o *.so *.a *.cma
