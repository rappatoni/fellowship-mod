#!/bin/sh
# Run AIDA from this worktree with the main checkout's virtualenv.
here=$(cd "$(dirname "$0")/.." && pwd)
export PYTHONPATH="$here" ACDC_NO_OPEN=1 ACDC_NO_RENDER=1 ACDC_FSP="$here/wrap/fellowship/fsp"
export ADF_BDD_BIN=/Users/maxrapp/Work/Uni/fellowship-mod/.venv/bin/adf-bdd
exec /Users/maxrapp/Work/Uni/fellowship-mod/.venv/bin/python -c 'import sys; from wrap.cli import main; sys.exit(main())' "$@"
