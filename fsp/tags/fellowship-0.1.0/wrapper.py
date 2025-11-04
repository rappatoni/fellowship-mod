from __future__ import annotations
import os
import sys
import logging

from wrap.prover import ProverWrapper, ProverError, MachinePayloadError
from core.dc.argument import Argument
from core.ac.grammar import Grammar, ProofTermTransformer

# Install TRACE level (below DEBUG) and a Logger.trace() method
TRACE = 5
logging.addLevelName(TRACE, "TRACE")
def _trace(self, msg, *args, **kwargs):
    if self.isEnabledFor(TRACE):
        self._log(TRACE, msg, args, **kwargs)
logging.Logger.trace = _trace

from wrap.cli import (
    configure_logging_cli,
    pop,
    execute_script,
    interactive_mode,
    setup_prover,
    reduce_argument_cmd,
    render_argument_cmd,
    color_argument_cmd,
    tree_argument_cmd,
    main,
)

__all__ = [
    "ProverWrapper", "ProverError", "MachinePayloadError", "Argument",
    "Grammar", "ProofTermTransformer",
    "configure_logging_cli", "pop", "execute_script", "interactive_mode",
    "setup_prover", "reduce_argument_cmd", "render_argument_cmd",
    "color_argument_cmd", "tree_argument_cmd", "main",
]

if __name__ == '__main__':
    main()
