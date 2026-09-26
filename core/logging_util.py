"""The TRACE level, owned by ``core`` rather than by the CLI.

Until 2026-09-26 ``TRACE = 5`` and the ``Logger.trace`` method were installed
by importing ``wrap/cli.py``, so a ``logger.trace(...)`` in a core module only
worked when the CLI happened to have been imported first - a dependency from
``core`` on ``wrap`` that nothing declared.  The registration lives here now;
``wrap/cli.py`` imports TRACE from this module, so the CLI behaves exactly as
before.

New code in the pipeline calls ``logger.log(TRACE, ...)``, which needs no
monkey-patch and is what ``wrap/prover.py`` already does.  ``Logger.trace`` is
still installed for the existing callers in ``core/dc/argument.py``.
"""

import logging

#: Below DEBUG: the step-by-step channel (prover I/O, reduction steps).
TRACE = 5

logging.addLevelName(TRACE, "TRACE")


def _trace(self, msg, *args, **kwargs):
    if self.isEnabledFor(TRACE):
        self._log(TRACE, msg, args, **kwargs)


if not hasattr(logging.Logger, "trace"):
    logging.Logger.trace = _trace
