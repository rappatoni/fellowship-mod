# sexp_parser.py — S‑expression reader for machine payloads (atoms and lists).
from __future__ import annotations

import re
from typing import Any, List, Tuple

_TOKEN = re.compile(rb"""
      \s*(                         # one token:
          "(?:[^"\\]|\\.)*"        #   double-quoted string with escapes
        | [()]                     #   a single parenthesis
        | [^()\s]+                 #   or a bare atom
      )
  """, re.VERBOSE)

class SexpParser:
    def parse(self, data: bytes) -> Any:
        self._tokens = [m.group(1) for m in _TOKEN.finditer(data)]
        self._idx = 0
        val, idx_end = self._read()
        if idx_end != len(self._tokens):
            raise ValueError("trailing tokens in s-expression")
        return val

    # --------------------------------------------------------------
    def _unescape(self, b: bytes) -> str:
        # machine.ml only escapes quotes; we handle \" and \\ defensively
        return b.replace(b'\\"', b'"').replace(b'\\\\', b'\\').decode('utf-8')

    def _read(self) -> Tuple[Any, int]:
        if self._idx >= len(self._tokens):
            raise ValueError("unexpected EOF in s-expression")
        tok = self._tokens[self._idx]
        self._idx += 1

        if tok == b"(":
            lst: List[Any] = []
            while True:
                if self._idx >= len(self._tokens):
                    raise ValueError("missing closing ')'")
                if self._tokens[self._idx] == b")":
                    self._idx += 1
                    break
                v, _ = self._read()
                lst.append(v)
            return lst, self._idx
        elif tok == b")":
            raise ValueError("unexpected ')'")
        elif tok.startswith(b'"'):
            # strip quotes and unescape
            return self._unescape(tok[1:-1]), self._idx
        else:
            # bare atom
            return tok.decode('utf-8'), self._idx
