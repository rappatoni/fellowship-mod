# Florent Kirchner			-- LogiCal
# Makefile


# Variable definition

RESULT=fsp
SOURCES=core.ml interpreter.ml interpreter_proof_terms.ml print.ml commands.ml help.ml parser.mly lexer.mll main.ml
LIBS=unix
TRASH=parser.output

-include OCamlMakefile

tests:
	@ (cd tests && for i in *.fsp; do ../fsp -c $$i > /dev/null 2> /dev/null && echo "$$i: OK" || echo "$$i: ERROR"; done)

cleantests:
	(cd tests && rm -f *.v *.pvs)

man: 
	(cd documentation && make)

cleanman:
	(cd documentation && make clean)

.PHONY: tests cleantests
