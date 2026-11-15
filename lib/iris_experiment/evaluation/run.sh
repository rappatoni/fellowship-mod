#!/bin/sh
# Re-run every comparison tool on the IRIS material (survey.org lists how
# each tool was installed). The tools live outside the repository; point
# at them with these variables:
#   CARNEADES   the carneades (version 4) binary
#   PYARG_PY    a python with python-argumentation 2.0.2 and clingo
#   ASPARTIX    directory holding the ASPARTIX Dung encodings (stable.dl)
#   ARG2P_PY    a python (3.12) with arg2p 0.26.5
#   SCASP       the s(CASP) binary (default: scasp on PATH)
#   DDL         a checkout of github.com/gvdgdo/Defeasible-Deontic-Logic
#   CLINGO      a clingo command (default: clingo)
#   CIAO_SHELL  ciao-shell (Ciao Prolog)
# Each tool's directory keeps the saved outputs of the run recorded in
# comparison.org.
set -e
cd "$(dirname "$0")"
echo "### Carneades";     ./carneades/run.sh | tee carneades/labels.txt
echo "### ASPIC+ (PyArg)"; (cd pyarg && "$PYARG_PY" aspic.py | tee output.txt)
echo "### Arg2P";         (cd arg2p && "$ARG2P_PY" run.py | tee output.txt && "$ARG2P_PY" processing.py | tee processing.txt && "$ARG2P_PY" features.py | tee features.txt)
echo "### s(CASP)";       (cd scasp && python3 gen.py && ./run.sh | tee labels.txt)
echo "### DDL";           ./ddl/run.sh | tee ddl/labels.txt
echo "### PROLEG core";   (cd proleg && "$CIAO_SHELL" proleg.pl 2>/dev/null | grep -v '^[{}]' | tee output.txt)
