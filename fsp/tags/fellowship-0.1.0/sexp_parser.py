# sexp_parser.py — S‑expression reader for machine payloads (atoms and lists).
from __future__ import annotations

import re
from typing import Any, List, Tuple

_TOKEN = re.compile(rb"\s*([()])|\s*([^()\s]+)")

class SexpParser:
    def parse(self, data: bytes) -> Any:
        self._tokens = list(_TOKEN.finditer(data))
        self._idx = 0
        val, idx_end = self._read()
        if idx_end != len(self._tokens):
            raise ValueError("trailing tokens in s-expression")
        return val

    # --------------------------------------------------------------
    def _read(self) -> Tuple[Any, int]:
        if self._idx >= len(self._tokens):
            raise ValueError("unexpected EOF in s-expression")
        m = self._tokens[self._idx]
        self._idx += 1
        g1, g2 = m.group(1), m.group(2)
        if g1 == b"(":
            # list
            lst: List[Any] = []
            while True:
                if self._idx >= len(self._tokens):
                    raise ValueError("missing closing ')'")
                if self._tokens[self._idx].group(1) == b")":
                    self._idx += 1
                    break
                v, _ = self._read()
                lst.append(v)
            return lst, self._idx
        elif g1 == b")":
            raise ValueError("unexpected ')'")
        else:
            # atom
            return g2.decode('utf-8'), self._idx
