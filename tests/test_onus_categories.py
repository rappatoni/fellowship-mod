from core.ac.ast import Goal, Laog, ID, DI, Mu, Mutilde
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

    # Helper: dap (defeated ap): t' is an exception node.
    # Use a typ-correct classic exception shape: μ_.<t' || t> where t' is t-prime and context is not ID.
    # Here: t' = Goal, context = Laog (not ID) satisfies _is_exception_node for Mu.
    def mk_dap_defeated():
        return Mu(
            id_=ID("_", "A"),
            prop="A",
            term=Mu(
                id_=ID("_", "A"),
                prop="A",
                term=Goal("1", "A"),
                context=Laog("L", "A"),
            ),
            context=ID("alpha", "A"),
        )

    ap = mk_ap_default()
    dap = mk_dap_defeated()

    # Build command nodes where left is a Term and right is a Context.
    # Put ap/dap on the term side; use an arbitrary context binder to host the other side.

    # ⟨ ap || dap ⟩ (encode right dap as a context-shaped node: μ'_.<t'||alpha>)
    dap_ctx = Mutilde(
        di_=DI("_", "A"),
        prop="A",
        term=dap.term,  # the defeated t' subtree
        context=ID("alpha", "A"),
    )
    node1 = Mu(id_=ID("gamma", "A"), prop="A", term=ap, context=dap_ctx)
    assert r._classify_for_onus(ap)[0] == "ap"
    action1, _reason1 = r._decide_onus(node1)

    # ⟨ dap || ap ⟩
    ap_ctx = Mutilde(
        di_=DI("_", "A"),
        prop="A",
        term=Goal("1", "A"),
        context=ID("alpha", "A"),
    )
    node2 = Mu(id_=ID("gamma", "A"), prop="A", term=dap, context=ap_ctx)
    action2, _reason2 = r._decide_onus(node2)

    assert action1 == "cbn"
    assert action2 == "cbv"
