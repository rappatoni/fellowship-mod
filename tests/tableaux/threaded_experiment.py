"""Hand-threaded debate term for the modus ponens tableau (experiment, 2026-11-10).

Not a test: a record of the experiment in tasks.org
(aida-tableau-correspondence, aida-tableau-threaded-unfolding).  Run from
the repository root:

    .venv/bin/python tests/tableaux/threaded_experiment.py tests/tableaux/modus_ponens.fspy

From the unfolder's own pieces for 'r1' it builds:
  (a) the unfolded term as is;
  (b) threaded: r1's and r2's demands held by branch binders, the
      expansion of the compound demand nested inside them;
  (c) threaded plus closing: i1's A[c] demand attacked by the branch binder
      holding A[t], i2's B[t] demand by the one holding B[c];
  (d) closing alone, on the unthreaded term (the binders are then free).
Each is type-checked through Fellowship and evaluated (skeptical, preferred).
Recorded result: (a), (b) type-check, 0 strict decisions, OPEN; (c)
type-checks, 2 strict decisions, still OPEN with the issue OUT; (d) fails
the type check.
"""
import copy
import logging
import sys
import warnings

from core.ac.ast import Mu, Mutilde, Cons, DI, ID, Goal, Laog
from core.comp.evaluate import evaluate_debate
from core.dc.debate_graph import declaration_kinds
from core.dc.typecheck import typecheck, TypeCheckFailed
from core.dc.unfold import argument_edge, unfold_argument
from pres.gen import pres_tree
from wrap.cli import execute_script, setup_prover

SCRIPT = sys.argv[1]
G, Q, AB, A, B = "A->(A->B)->B", "(A->B)->B", "A->B", "A", "B"


def children(n):
    for attr in ("term", "context"):
        c = getattr(n, attr, None)
        if c is not None and not isinstance(c, str):
            yield attr, c


def find(n, pred, parent=None, attr=None):
    """(node, parent, attr) of the first node satisfying pred, depth first."""
    if pred(n):
        return n, parent, attr
    for a, c in children(n):
        r = find(c, pred, n, a)
        if r:
            return r
    return None


def named(kind, name):
    def pred(n):
        if kind == "mu":
            return isinstance(n, Mu) and n.id.name == name
        return isinstance(n, Mutilde) and n.di.name == name
    return pred


def attack_on_site(n, site_cls, prop):
    """The attack scaffold whose original is a bare site of the given kind."""
    if site_cls is Laog:
        return isinstance(n, Mutilde) and n.prop == prop and isinstance(n.context, Laog) \
            and n.di.name.startswith("alt")
    return isinstance(n, Mu) and n.prop == prop and isinstance(n.term, Goal) \
        and n.id.name.startswith("alt")


def close(term):
    """Write the two closing captures into the scaffolds of i1's and i2's demands."""
    i1 = find(term, named("mu", "i1"))[0]
    sc = find(i1, lambda n: attack_on_site(n, Laog, A))[0]
    # context side, C-ATT: mu'alt.< mu b.< mu_.<alt||b> || mu'_.<SCION||b> > || site >
    sc.term.context.term = DI("yA", A)
    i2 = find(term, named("mu", "i2"))[0]
    sc = find(i2, lambda n: attack_on_site(n, Goal, B))[0]
    # term side, T-ATT: mu alt.< site || mu'b.< mu_.<b||SCION> || mu'_.<b||alt> > >
    sc.context.term.context = ID("bB", B)
    return term


def thread(term):
    """Hold r1's and r2's demands by branch binders over the compound demand."""
    r1, r1_parent, r1_attr = find(term, named("mutilde", "r1"))
    d_a, d_q = r1.context.term, r1.context.context          # r1 = mu'r1.<r1 || D_A * D_Q>
    r2, r2_parent, r2_attr = find(d_q, named("mutilde", "r2"))
    d_ab, d_b = r2.context.term, r2.context.context         # r2 = mu'r2.<r2 || D_AB * D_B>
    # r2 threaded:  mu'gQ:Q.< mu bB:B.< D_AB || mu'fAB.< gQ || mu'r2.<r2 || fAB*bB> > > || D_B >
    r2_inner = Mutilde(DI("r2", Q), Q, DI("r2", Q), Cons(DI("fAB", AB), ID("bB", B)))
    r2t = Mutilde(DI("gQ", Q), Q,
                  Mu(ID("bB", B), B, d_ab, Mutilde(DI("fAB", AB), AB, DI("gQ", Q), r2_inner)),
                  d_b)
    setattr(r2_parent, r2_attr, r2t)
    # r1 threaded:  mu'gG:G.< D_A || mu'yA:A.< mu qQ:Q.< gG || mu'r1.<r1 || yA*qQ> > || D_Q > >
    r1_inner = Mutilde(DI("r1", G), G, DI("r1", G), Cons(DI("yA", A), ID("qQ", Q)))
    r1t = Mutilde(DI("gG", G), G, d_a,
                  Mutilde(DI("yA", A), A, Mu(ID("qQ", Q), Q, DI("gG", G), r1_inner), d_q))
    setattr(r1_parent, r1_attr, r1t)
    return term


def main():
    logging.basicConfig(level=logging.WARNING)
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(p, SCRIPT, strict=False, stop_on_error=False, isolate=False,
                       render_files=False, stop_marker=False)
    base = unfold_argument(p.graph, argument_edge(p.graph, "r1"))
    variants = {
        "a_unfolded": copy.deepcopy(base),
        "b_threaded": thread(copy.deepcopy(base)),
        "c_threaded_closed": close(thread(copy.deepcopy(base))),
        "d_closed_unthreaded": close(copy.deepcopy(base)),
    }
    opts = dict(strict_names=p.declarations.keys(), strict_kinds=declaration_kinds(p.declarations),
                source=p.graph)
    for name, term in variants.items():
        print(f"\n===== {name}")
        print(pres_tree(term))
        try:
            typecheck(p, copy.deepcopy(term), f"tc_{name}", G, True)
            print("typecheck: OK")
        except TypeCheckFailed as e:
            print(f"typecheck: FAILED: {e}")
        except Exception as e:  # noqa: BLE001 - report and go on
            print(f"typecheck: ERROR {type(e).__name__}: {e}")
        try:
            nf, cls, sigma, _g = evaluate_debate(copy.deepcopy(term), "r1", mode="skeptical", **opts)
            print("sigma:", "  ".join(f"{k[0]}[{k[1][0]}]={v}" for k, v in sigma.items()))
            print("class:", cls)
            print("normal form:\n" + pres_tree(nf))
        except Exception as e:  # noqa: BLE001
            print(f"evaluate: ERROR {type(e).__name__}: {e}")
    p.close()


if __name__ == "__main__":
    main()
