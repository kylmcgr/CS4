default: compile

NAME = lab2

all: compile test

compile: clean
	ocamlfind ocamlc -g -c -package num ${NAME}.mli ${NAME}.ml

test: compile
	ocamlfind ocamlc -g -o tests_${NAME} \
	  -package ounit2,num -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

clean:
	rm -f *.cm* *.log *.cache tests_${NAME}
