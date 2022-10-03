default: compile

NAME = lab3

all: compile test

compile: clean
	ocamlfind ocamlc -c -package num ${NAME}.mli ${NAME}.ml

test:
	ocamlfind ocamlc -o tests_${NAME} \
	  -package ounit2,num -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

clean:
	rm -f *.cm* *.log *.cache tests_${NAME}
