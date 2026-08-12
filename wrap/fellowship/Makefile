# Florent Kirchner			-- LogiCal
# Makefile
# Modifications Copyright (C) 2025-2026 Max Rapp
# The modifications are licensed under GPL-3.0-only. This combined file is
# distributed under GPL-3.0-only as permitted by CeCILL v2.0 Article 5.3.4;
# the original CeCILL notice and terms remain applicable to the original work.


# Variable definition

RESULT=fsp
SOURCES= core.ml machine.ml interpreter.ml interpreter_proof_terms.ml print.ml tactics.ml instructions.ml help.ml parser.mly lexer.mll main.ml 
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
