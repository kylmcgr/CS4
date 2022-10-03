SOLVE     = klotski_solve
INTER     = klotski_interact
TESTS     = tests_lab7

MLI_BASE  = boards.mli klotski.mli
ML_BASE	  = boards.ml klotski.ml
CMO_BASE  = boards.cmo klotski.cmo
CMX_BASE  = boards.cmx klotski.cmx

MLI_SOLVE = ${MLI_BASE} search.mli 
ML_SOLVE  = ${ML_BASE} search.ml solve.ml
CMO_SOLVE = ${CMO_BASE} search.cmo solve.cmo
CMX_SOLVE = ${CMX_BASE} search.cmx solve.cmx

MLI_INTER = ${MLI_BASE}
ML_INTER  = ${ML_BASE} interact.ml
CMO_INTER = ${CMO_BASE} interact.cmo

default: solve_native interact

solve:
	ocamlfind ocamlc -c ${MLI_SOLVE}
	ocamlfind ocamlc -c ${ML_SOLVE}
	ocamlfind ocamlc ${CMO_SOLVE} -o ${SOLVE}

solve_native:
	ocamlfind ocamlc -c ${MLI_SOLVE}
	ocamlfind ocamlopt -c ${ML_SOLVE}
	ocamlfind ocamlopt ${CMX_SOLVE} -o ${SOLVE}

interact:
	ocamlfind ocamlc -c ${MLI_INTER}
	ocamlfind ocamlc -c ${ML_INTER}
	ocamlfind ocamlc ${CMO_INTER} -o ${INTER}

test: solve
	ocamlfind ocamlc -o ${TESTS} \
	  -package ounit2,str -linkpkg \
	  ${CMO_BASE} ${TESTS}.ml
	./${TESTS}

clean:
	rm -f a.out *.cm* *.o ${SOLVE} ${INTER}
	rm -f *.log *.cache ${TESTS}

