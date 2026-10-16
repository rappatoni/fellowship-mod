"""Standing: what an expression of a labelled debate term comes to *in
the term*, as opposed to its statement's label (tasks.org,
aida-judge-by-what-it-stands-for; decided by the author 2026-10-16).

The principle: an expression is judged by what it stands for, not by
its statement's label.  Sigma (core/comp/evaluate.py,
``resolve_scaffolds``) decides a scaffold by the *standing* of its scion,
computed bottom-up from the term:

- a nested debate stands for how it comes out here: its scaffolds are
  decided by the same rule, and the debate's standing is that of the
  wing kept (an attack won: OUT);
- a scaffold's ``b`` stands for the original it binds after the outer
  step - the root, or the part of the stack below; a root that is an
  obligation stands for an obligation nobody met in the term: OUT; a
  presumption root keeps its statement's label (somebody presumed it);
- a scaffold's ``alt`` - the captured root of the contrary, the
  attacker wing's root - follows the same root rule: unmet (OUT) unless
  the captured demand was a presumption (``captured_presumption`` or a
  presumption among ``captured_default``), then its label;
- a variable bound by an argument's own binder, captured inside another
  argument, keeps its statement's label (case 4 of the task, decided
  elsewhere: aida-discuss-capture-source-vs-strict); one used inside the
  argument that binds it is plumbing and counts for nothing;
- an obligation site OUT, a presumption site its label; declared names
  and built-in leaves count for nothing;
- a derivation stands IN iff all it rests on is IN, OUT iff something
  is OUT, UNDEC otherwise (the three-valued conjunction
  ``derivation_status`` uses).

Labels therefore decide only what the term cannot: presumptions, and
the variables of case 4.  The discharge of phase 3 is unchanged: a root
that survives evaluation stands for its statement and is named by its
label.
"""

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Goal, Laog, Deleg, Geled, ID, DI,
)
from core.dc.debate_graph import _match_scaffold, _is_affine_binder, BUILTIN_LEAVES

IN, OUT, UNDEC = "IN", "OUT", "UNDEC"

#: Entries of a standing environment, per variable name.
OWN = ("own",)          # bound inside the derivation being read: plumbing
LABEL = ("label",)      # an argument's binder, crossed into another argument
ROOT = ("root",)        # a scaffold's alt: the captured root of the contrary


def conj(values) -> str:
    """The three-valued conjunction: OUT if any is OUT, else UNDEC if any
    is UNDEC, else IN (the empty conjunction is IN)."""
    values = [v for v in values if v is not None]
    if OUT in values:
        return OUT
    if UNDEC in values:
        return UNDEC
    return IN


def _root_rule(node) -> str:
    """A captured root: unmet unless the demand captured was a presumption."""
    presumed = (getattr(node, "captured_presumption", False)
                or "presumption" in getattr(node, "captured_default", ()))
    return (getattr(node, "label", None) or UNDEC) if presumed else OUT


def crossed(env: dict) -> dict:
    """The environment as seen from inside a scaffold's part: the binders
    of the enclosing argument now capture (``LABEL``)."""
    return {k: (LABEL if v is OWN else v) for k, v in env.items()}


def wing_choice(role: str, status: str, mode: str) -> str:
    """"scion" or "orig" (the table of core/comp/evaluate.py)."""
    if role == "supporter":
        if status == IN:
            return "scion"
        if status == OUT:
            return "orig"
        return "scion" if mode == "credulous" else "orig"
    if status == IN:
        return "scion"
    if status == OUT:
        return "orig"
    return "orig" if mode == "credulous" else "scion"


class Standing:
    """Standings of a labelled term under a mode (the UNDEC tiebreak of
    sigma's table).  ``stubs``: an ``IssueResolver`` for the shared route,
    whose stubs stand for resolved instances (core/dc/instances.py)."""

    def __init__(self, mode: str, strict_names=(), stubs=None, labels=None):
        self.mode = mode
        self.labels = labels or {}
        self.strict_names = set(strict_names or ()) | set(BUILTIN_LEAVES)
        self.stubs = stubs
        self._memo = {}

    # -- scaffolds ------------------------------------------------------------

    def scaffold(self, match, env: dict):
        """(status of the scion, standing of the original, choice, standing
        of the whole) for a matched scaffold under ``env``."""
        role, _side, _prop, orig, scion, alt, _kind = match
        inner = crossed(env)
        inner[alt] = ROOT
        s_orig = self.of(orig, inner)
        scion_env = dict(inner)
        if getattr(match, "beta", None):
            scion_env[match.beta] = ("std", s_orig)
        s_scion = self.of(scion, scion_env)
        target = getattr(scion, "label", None)
        status = conj([target, s_scion]) if target else s_scion
        choice = wing_choice(role, status, self.mode)
        if choice == "orig":
            whole = s_orig
        elif role == "supporter":
            whole = status
        else:
            whole = OUT if status == IN else UNDEC
        return status, s_orig, choice, whole

    # -- expressions ----------------------------------------------------------

    def of(self, node, env: dict) -> str:
        """The standing of ``node`` under ``env`` ({name: entry})."""
        if node is None or not isinstance(node, ProofTerm):
            return IN
        if self.stubs is not None and type(node).__name__ == "Stub":
            return self._stub(node, env)
        match = _match_scaffold(node, self.strict_names)
        if match is not None:
            return self.scaffold(match, env)[3]
        if isinstance(node, (Goal, Laog)):
            return OUT
        if isinstance(node, (Deleg, Geled)):
            return getattr(node, "label", None) or UNDEC
        if isinstance(node, (ID, DI)):
            return self._variable(node, env)
        if isinstance(node, (Mu, Mutilde, Lamda, Admal)):
            env = {**env, _binder(node): OWN}
        return conj(self.of(getattr(node, slot, None), env) for slot in ("term", "context")
                    if isinstance(getattr(node, slot, None), ProofTerm))

    def _variable(self, node, env) -> str:
        if _is_affine_binder(node.name) or node.name in self.strict_names:
            return IN
        entry = env.get(node.name)
        if entry is None or entry is OWN:
            return IN
        if entry is LABEL:
            return getattr(node, "label", None) or IN
        if entry is ROOT:
            return _root_rule(node)
        return entry[1]                                  # ("std", standing)

    def _stub(self, stub, env) -> str:
        """A stub stands for its resolved instance, under the standings its
        captured variables have here."""
        from core.dc.instances import _param
        from core.comp.labelled import label_term
        resolved = self.stubs.resolve(stub.instance).resolved
        defaults = self.stubs.shared.graph.defaults
        params = {}
        for statement, actual in stub.actual.items():
            entry = env.get(actual)
            label = self.labels.get(statement) or UNDEC
            if entry is None or entry is OWN:
                value = IN
            elif entry is LABEL:
                value = label
            elif entry is ROOT:
                value = label if "presumption" in defaults.get(statement, ()) else OUT
            else:
                value = entry[1]
            params[_param(statement)] = ("std", value)
        key = (stub.instance.key, tuple(sorted((k, v[1]) for k, v in params.items())))
        if key not in self._memo:
            self._memo[key] = self.of(label_term(resolved, self.labels), params)
        return self._memo[key]


def _binder(node) -> str:
    if isinstance(node, Mu):
        return node.id.name
    if isinstance(node, Mutilde):
        return node.di.name
    if isinstance(node, Lamda):
        return node.di.di.name
    return node.id.id.name


__all__ = ["Standing", "conj", "crossed", "wing_choice", "IN", "OUT", "UNDEC",
           "OWN", "LABEL", "ROOT"]
