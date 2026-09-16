"""Naive ADF oracle: trust level 3 of propositional-fragment-plan.org.

This module transcribes the abstract-dialectical-framework definitions used
by debate-graph-spec.org as directly as possible.  It is *deliberately*
exponential: interpretations are enumerated, the characteristic operator is
evaluated by enumerating completions, and the grounded interpretation is
found by filtering, not by fixpoint iteration.  Correctness by inspection
is the design goal; do not optimize this module, and do not import
production algorithms into it.

Three-valued interpretations map statements to True, False, or None, where
None means undecided (UNDEC).

Also provides the exporter to the DIAMOND-family input format consumed by
the third-party solvers (adf-bdd, DIAMOND, YADF) used as the independent
cross-check.
"""

import os
import re
import shutil
import subprocess
import tempfile
from itertools import product
from typing import Mapping, Optional

# ---------------------------------------------------------------------------
# Formulas
#
# A formula is a nested tuple:
#   ("const", bool)
#   ("var", statement_name)
#   ("not", formula)
#   ("and", (formula, ...))     empty conjunction is true
#   ("or", (formula, ...))      empty disjunction is false
# ---------------------------------------------------------------------------

def const(value: bool):
    return ("const", bool(value))

def var(name: str):
    return ("var", name)

def neg(f):
    return ("not", f)

def conj(*fs):
    return ("and", tuple(fs))

def disj(*fs):
    return ("or", tuple(fs))


def eval_formula(f, valuation: Mapping[str, bool]) -> bool:
    """Two-valued evaluation of a formula under a total valuation."""
    tag = f[0]
    if tag == "const":
        return f[1]
    if tag == "var":
        return valuation[f[1]]
    if tag == "not":
        return not eval_formula(f[1], valuation)
    if tag == "and":
        return all(eval_formula(g, valuation) for g in f[1])
    if tag == "or":
        return any(eval_formula(g, valuation) for g in f[1])
    raise ValueError(f"Unknown formula tag: {tag!r}")


def formula_vars(f) -> set:
    tag = f[0]
    if tag == "const":
        return set()
    if tag == "var":
        return {f[1]}
    if tag == "not":
        return formula_vars(f[1])
    if tag in ("and", "or"):
        out = set()
        for g in f[1]:
            out |= formula_vars(g)
        return out
    raise ValueError(f"Unknown formula tag: {tag!r}")


# ---------------------------------------------------------------------------
# ADFs
# ---------------------------------------------------------------------------

#: Statement-count guard: 3^n interpretations times 2^n completions each.
MAX_ORACLE_STATEMENTS = 14


class ADF:
    """An abstract dialectical framework: statements + acceptance conditions.

    ``statements`` fixes the order used everywhere (export, enumeration).
    ``ac`` maps every statement to its acceptance condition, a formula over
    (a subset of) the statements.
    """

    def __init__(self, statements, ac, *, guard: bool = True):
        """``guard=False`` lifts the statement-count limit.  Only the
        adf-bdd path may do that: the enumeration functions below are
        exponential and must never see an unguarded large ADF."""
        self.statements = tuple(statements)
        self.ac = dict(ac)
        if len(set(self.statements)) != len(self.statements):
            raise ValueError("Duplicate statement names")
        missing = [s for s in self.statements if s not in self.ac]
        if missing:
            raise ValueError(f"Statements without acceptance condition: {missing}")
        extra = [s for s in self.ac if s not in self.statements]
        if extra:
            raise ValueError(f"Acceptance conditions for unknown statements: {extra}")
        for s in self.statements:
            unknown = formula_vars(self.ac[s]) - set(self.statements)
            if unknown:
                raise ValueError(
                    f"Acceptance condition of {s!r} mentions unknown statements: {sorted(unknown)}"
                )
        if guard and len(self.statements) > MAX_ORACLE_STATEMENTS:
            raise ValueError(
                f"Naive oracle is limited to {MAX_ORACLE_STATEMENTS} statements "
                f"(got {len(self.statements)}); it enumerates interpretations "
                f"by design and is meant for fixture-scale graphs only."
            )

    # -- parents / acyclicity ------------------------------------------------

    def parents(self, s: str) -> set:
        return formula_vars(self.ac[s])

    def is_acyclic(self) -> bool:
        """True iff the parent relation has no directed cycle (DFS, three colors)."""
        WHITE, GRAY, BLACK = 0, 1, 2
        color = {s: WHITE for s in self.statements}

        def visit(s) -> bool:
            color[s] = GRAY
            for p in self.parents(s):
                if color[p] == GRAY:
                    return False
                if color[p] == WHITE and not visit(p):
                    return False
            color[s] = BLACK
            return True

        return all(visit(s) for s in self.statements if color[s] == WHITE)


# ---------------------------------------------------------------------------
# Two-valued semantics
# ---------------------------------------------------------------------------

def two_valued_interpretations(adf: ADF):
    """All total valuations statement -> bool, in a fixed order."""
    for bits in product((True, False), repeat=len(adf.statements)):
        yield dict(zip(adf.statements, bits))


def two_valued_models(adf: ADF):
    """Valuations v with v(s) = v(AC_s) for every statement s."""
    return [
        v
        for v in two_valued_interpretations(adf)
        if all(v[s] == eval_formula(adf.ac[s], v) for s in adf.statements)
    ]


# ---------------------------------------------------------------------------
# Three-valued semantics
# ---------------------------------------------------------------------------

def three_valued_interpretations(adf: ADF):
    for values in product((True, False, None), repeat=len(adf.statements)):
        yield dict(zip(adf.statements, values))


def completions(adf: ADF, v: Mapping[str, Optional[bool]]):
    """All two-valued valuations agreeing with v on its decided statements."""
    undecided = [s for s in adf.statements if v[s] is None]
    for bits in product((True, False), repeat=len(undecided)):
        w = dict(v)
        w.update(zip(undecided, bits))
        yield w


def gamma(adf: ADF, v: Mapping[str, Optional[bool]]):
    """The characteristic (consensus) operator.

    gamma(v)(s) is True if AC_s holds under every completion of v, False if
    it fails under every completion, and None otherwise.
    """
    out = {}
    for s in adf.statements:
        results = {eval_formula(adf.ac[s], w) for w in completions(adf, v)}
        out[s] = True if results == {True} else False if results == {False} else None
    return out


def complete_interpretations(adf: ADF):
    """Three-valued v with gamma(v) = v."""
    return [v for v in three_valued_interpretations(adf) if gamma(adf, v) == v]


def leq_information(v, w) -> bool:
    """v <=_i w: everywhere v is decided, w agrees."""
    return all(v[s] is None or v[s] == w[s] for s in v)


def grounded_interpretation(adf: ADF):
    """The <=_i-least complete interpretation.

    Found by filtering, per the definition: among all complete
    interpretations, the one below all others.  Uniqueness and existence
    are asserted, not assumed silently.
    """
    complete = complete_interpretations(adf)
    least = [v for v in complete if all(leq_information(v, w) for w in complete)]
    if len(least) != 1:
        raise AssertionError(
            f"Grounded interpretation not unique: found {len(least)} <=_i-least "
            f"complete interpretations (theory guarantees exactly one)."
        )
    return least[0]


# ---------------------------------------------------------------------------
# DIAMOND-format export (input format of adf-bdd / DIAMOND / YADF)
#
# Statements become s(s1). ... in the order of adf.statements; acceptance
# conditions become ac(si, <prefix formula>). with binary and/or, neg, and
# the constants c(v) / c(f).  Arbitrary proposition strings are mapped to
# safe atoms s1..sn; the mapping is returned alongside the text.
# ---------------------------------------------------------------------------

def _diamond_formula(f, name_map: Mapping[str, str]) -> str:
    tag = f[0]
    if tag == "const":
        return "c(v)" if f[1] else "c(f)"
    if tag == "var":
        return name_map[f[1]]
    if tag == "not":
        return f"neg({_diamond_formula(f[1], name_map)})"
    if tag == "and":
        parts = [_diamond_formula(g, name_map) for g in f[1]]
        if not parts:
            return "c(v)"
        out = parts[-1]
        for p in reversed(parts[:-1]):
            out = f"and({p},{out})"
        return out
    if tag == "or":
        parts = [_diamond_formula(g, name_map) for g in f[1]]
        if not parts:
            return "c(f)"
        out = parts[-1]
        for p in reversed(parts[:-1]):
            out = f"or({p},{out})"
        return out
    raise ValueError(f"Unknown formula tag: {tag!r}")


def diamond_export(adf: ADF):
    """Render the ADF in DIAMOND input format.

    Returns (text, name_map) where name_map maps each statement to the safe
    atom (s1..sn) used in the text.
    """
    name_map = {s: f"s{i + 1}" for i, s in enumerate(adf.statements)}
    lines = [f"s({name_map[s]})." for s in adf.statements]
    for s in adf.statements:
        lines.append(f"ac({name_map[s]},{_diamond_formula(adf.ac[s], name_map)}).")
    return "\n".join(lines) + "\n", name_map


# ---------------------------------------------------------------------------
# Third-party cross-check harness: adf-bdd (ellmau/adf-obdd)
#
# CLI and output format locked against adf-bdd-bin 0.3.0:
#   adf-bdd -q --lx --grd <file>   one line:  u(s1) T(s2) F(s3)
#   adf-bdd -q --lx --com <file>   one such line per complete model
#   adf-bdd -q --lx --stm <file>   one such line per stable model
# ---------------------------------------------------------------------------

_ADF_BDD_MODES = {"grounded": "--grd", "complete": "--com", "stable": "--stm"}
_ADF_BDD_TOKEN = re.compile(r"([TFu])\((\w+)\)")
_ADF_BDD_VALUE = {"T": True, "F": False, "u": None}


ADF_BDD_SEARCH_ORDER = (
    "ADF_BDD_BIN",
    "<active venv>/bin/adf-bdd",
    "./.venv/bin/adf-bdd",
    "adf-bdd on PATH",
    "~/.cargo/bin/adf-bdd",
)


def find_adf_bdd() -> Optional[str]:
    """Path to the adf-bdd binary, or None.  Mirrors the Fellowship
    binary's resolution: environment variable, packaged location (the
    active virtual environment, where `make install` puts it), repo-local
    venv, PATH, then a plain `cargo install` location."""
    import sys
    env = os.environ.get("ADF_BDD_BIN")
    if env and os.path.isfile(env) and os.access(env, os.X_OK):
        return env
    for candidate in (
        os.path.join(sys.prefix, "bin", "adf-bdd"),
        os.path.join(os.getcwd(), ".venv", "bin", "adf-bdd"),
    ):
        if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return candidate
    on_path = shutil.which("adf-bdd")
    if on_path:
        return on_path
    cargo = os.path.expanduser("~/.cargo/bin/adf-bdd")
    if os.path.isfile(cargo) and os.access(cargo, os.X_OK):
        return cargo
    return None


class AdfBddNotFound(RuntimeError):
    """The labeller needs adf-bdd and it is not installed.  No fallback."""


def resolve_adf_bdd() -> str:
    """Path to adf-bdd, or raise AdfBddNotFound with the search order."""
    found = find_adf_bdd()
    if found is None:
        raise AdfBddNotFound(
            "adf-bdd binary not found. Run `make install` (or `make adf-bdd`) "
            "to build it into the venv, or set ADF_BDD_BIN. Searched: "
            + ", ".join(ADF_BDD_SEARCH_ORDER)
            + ". There is no fallback labeller."
        )
    return found


def run_adf_bdd(adf: ADF, mode: str = "grounded", binary: Optional[str] = None):
    """Run adf-bdd on the exported ADF; interpretations on original names.

    Returns a list of three-valued interpretations (dicts to True/False/None).
    Raises RuntimeError if the binary is missing or its output is not in the
    locked format — the cross-check must never silently pass.
    """
    if mode not in _ADF_BDD_MODES:
        raise ValueError(f"mode must be one of {sorted(_ADF_BDD_MODES)}, got {mode!r}")
    binary = binary or resolve_adf_bdd()
    if not (os.path.isfile(binary) and os.access(binary, os.X_OK)):
        raise AdfBddNotFound(f"adf-bdd binary is not executable: {binary}")
    text, name_map = diamond_export(adf)
    reverse = {v: k for k, v in name_map.items()}
    with tempfile.NamedTemporaryFile("w", suffix=".adf", delete=False) as fh:
        fh.write(text)
        path = fh.name
    try:
        proc = subprocess.run(
            # --lib naive: measured 2026-09-16, the default hybrid backend
            # grows ~quartically on plain chains (9.8 s at 301 statements)
            # while naive takes 178 ms with identical output.
            [binary, "-q", "--lx", "--lib", "naive", _ADF_BDD_MODES[mode], path],
            capture_output=True, text=True, timeout=120,
        )
    finally:
        os.unlink(path)
    if proc.returncode != 0:
        raise RuntimeError(f"adf-bdd failed ({proc.returncode}): {proc.stderr.strip()}")
    interpretations = []
    for line in proc.stdout.splitlines():
        tokens = _ADF_BDD_TOKEN.findall(line)
        if not tokens:
            continue
        v = {reverse[atom]: _ADF_BDD_VALUE[mark] for mark, atom in tokens}
        if set(v) != set(adf.statements):
            raise RuntimeError(
                f"adf-bdd output line does not cover all statements: {line!r}"
            )
        interpretations.append(v)
    if not interpretations and mode == "grounded":
        raise RuntimeError(f"adf-bdd produced no grounded model: {proc.stdout!r}")
    return interpretations
