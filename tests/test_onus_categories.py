import pytest

from core.ac.ast import Goal, Laog, ID, DI, Mu, Mutilde, Cons
from core.comp.reduce import ArgumentTermReducer


def test_onus_is_ap_mu_basic():
    r = ArgumentTermReducer(verbose=False, evaluation_discipline="legacy")
    ap = Mu(
        id_=ID("_", "A"),
        prop="A",
        term=Goal("1", "A"),
        context=ID("alpha", "A"),
    )
    is_ap, is_default, is_defeated = r._is_ap_node(ap)
    assert is_ap is True
    assert is_default is True
    assert is_defeated is False


def test_onus_is_ap_mutilde_basic():
    r = ArgumentTermReducer(verbose=False, evaluation_discipline="legacy")
    ap = Mutilde(
        di_=DI("_", "A"),
        prop="A",
        term=Goal("1", "A"),
        context=ID("alpha", "A"),
    )
    is_ap, is_default, is_defeated = r._is_ap_node(ap)
    assert is_ap is True
    assert is_default is True
    assert is_defeated is False


@pytest.mark.xfail(
    strict=True,
    reason=(
        "B-0017 expectation against the superseded per-critical-pair onus "
        "spec (M1 triage, 2026-08-31): _classify_for_onus misclassifies the "
        "dap context as 'ap', and the local pairwise decision this test "
        "checks is superseded by the label-guided Layer 4 discipline "
        "(call-by-onus.org; propositional-fragment-plan.org M6). Kept as a "
        "strict xfail so any behaviour change in the legacy path is loud; "
        "to be retired or rewritten during the M6 audit."
    ),
)
def test_onus_decide_ap_vs_dap_is_cbn_and_dap_vs_ap_is_cbv():
    """Spec check for B-0017:
    - ⟨ ap || dap ⟩ should pick call-by-name
    - ⟨ dap || ap ⟩ should pick call-by-value

    We encode dap by making the t' part an exception (so _is_ap_node returns defeated).
    """

    r = ArgumentTermReducer(verbose=False, evaluation_discipline="onus-parallel")

    # Helper: ap (default) is μ_.<Goal || ID(alpha)> with affine '_' binder
    def mk_ap_default():
        return Mu(
            id_=ID("_", "A"),
            prop="A",
            term=Goal("1", "A"),
            context=ID("alpha", "A"),
        )

    # Helper: defeated alternative proof (dap) for the RHS context:
    #   μ'_:A.< μ_.<2:A || μ'_:A.<2:A || μ'beta:A.<4:B->A|| 3:B * beta> > || alpha >
    # i.e. an ap-shape whose t' is a (narrow) exception.
    def mk_dap_ctx():
        # μ'beta:A.<4:B->A|| 3:B * beta>
        inner_mox = Mutilde(
            di_=DI("beta", "A"),
            prop="A",
            term=Goal("4", "B->A"),
            context=Cons(Goal("3", "B"), ID("beta", "A")),
        )
        # μ_:A.<2:A || μ'beta:A.<4:B->A|| 3:B * beta> >
        inner_exc = Mu(
            id_=ID("_", "A"),
            prop="A",
            term=Goal("2", "A"),
            context=inner_mox,
        )
        # μ'_:A.< inner_exc || alpha >
        return Mutilde(
            di_=DI("_", "A"),
            prop="A",
            term=inner_exc,
            context=ID("alpha", "A"),
        )

    ap = mk_ap_default()
    dap_ctx = mk_dap_ctx()

    # Build command nodes where left is a Term and right is a Context.
    # Put ap/dap on the term side; use an arbitrary context binder to host the other side.

    # ⟨ ap || dap ⟩
    node1 = Mu(id_=ID("gamma", "A"), prop="A", term=ap, context=dap_ctx)
    assert r._classify_for_onus(ap)[0] == "d"
    assert r._classify_for_onus(dap_ctx)[0] == "dap"
    action1, _reason1 = r._decide_onus(node1)

    # ⟨ dap || ap ⟩  (term-side dap mirrors mk_dap_ctx with a Mu root; the
    # original test referenced an undefined `dap` here and could never have
    # run past the assertion above — see the xfail reason.)
    dap_term = Mu(
        id_=ID("_", "A"),
        prop="A",
        term=Mu(
            id_=ID("_", "A"),
            prop="A",
            term=Goal("2", "A"),
            context=Mutilde(
                di_=DI("beta", "A"),
                prop="A",
                term=Goal("4", "B->A"),
                context=Cons(Goal("3", "B"), ID("beta", "A")),
            ),
        ),
        context=ID("alpha", "A"),
    )
    ap_ctx = Mutilde(
        di_=DI("_", "A"),
        prop="A",
        term=Goal("1", "A"),
        context=ID("alpha", "A"),
    )
    node2 = Mu(id_=ID("gamma", "A"), prop="A", term=dap_term, context=ap_ctx)
    action2, _reason2 = r._decide_onus(node2)

    assert action1 == "cbn"
    assert action2 == "cbv"
