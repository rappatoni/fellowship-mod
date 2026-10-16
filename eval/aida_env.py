"""Shared setup for the evaluation harness: run AIDA from this worktree."""
import os, sys
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, ROOT)
os.environ.setdefault("ACDC_NO_OPEN", "1")
os.environ.setdefault("ACDC_NO_RENDER", "1")
os.environ.setdefault("ACDC_FSP", os.path.join(ROOT, "wrap/fellowship/fsp"))
os.environ.setdefault("ADF_BDD_BIN", "/Users/maxrapp/Work/Uni/fellowship-mod/.venv/bin/adf-bdd")
import logging
logging.disable(logging.WARNING)
from wrap.cli import setup_prover
from wrap.service import Service
