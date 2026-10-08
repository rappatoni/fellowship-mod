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


def artifact(logger, label: str, text, level=logging.DEBUG) -> None:
    """Log one intermediate ARTIFACT of the pipeline: a term, a graph, a
    labelling - the thing a stage hands to the next one.

    The decision lines say what was chosen; these say what was chosen
    *from* and what came out, which is what an implementation is verified
    against.  Multi-line text is indented under the label, so the CLI's
    message-only formatter renders it as a block.

    The caller must have checked ``logger.isEnabledFor(level)`` first when
    building ``text`` costs anything - rendering a proof term deep-copies
    it (``pres.gen.pres_str``).
    """
    if not logger.isEnabledFor(level):
        return
    body = text if isinstance(text, str) else str(text)
    lines = body.splitlines() or [""]
    if len(lines) == 1:
        logger.log(level, "%s:", label)
        logger.log(level, "    %s", lines[0])
        return
    logger.log(level, "%s:", label)
    for line in lines:
        logger.log(level, "    %s", line)
