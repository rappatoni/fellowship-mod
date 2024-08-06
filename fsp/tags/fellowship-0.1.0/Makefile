# Florent Kirchner			-- LogiCal
# Makefile


# Variable definition

RESULT=fsp
SOURCES=core.ml interpreter.ml interpreter_proof_terms.ml print.ml tactics.ml instructions.ml help.ml parser.mly lexer.mll main.ml
LIBS=unix
TRASH=parser.output

-include OCamlMakefile

tests:
	@ (cd tests && for i in `find . -name "*.fsp" | sort`; do ../fsp -c $$i > $$i.out 2>&1 && echo "$$i: OK" || echo "$$i: ERROR"; done)

cleantests:
	(cd tests && rm -f *.v *.pvs *.out *.nl)

man: 
	(cd documentation && make)

cleanman:
	(cd documentation && make clean)

.PHONY: tests cleantests
