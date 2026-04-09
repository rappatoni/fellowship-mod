from core.ac.ast import Goal, Laog, ID, DI, Mu, Mutilde, Lamda, Hyp
from core.comp.reduce import ThetaExpander


A = "P"
B = "P->Q"
C = "(P->Q)->P"


def _term_expander() -> ThetaExpander:
    return ThetaExpander(A, mode="term")


def _context_expander() -> ThetaExpander:
    return ThetaExpander(A, mode="context")


def test_bureaucratic_schema_1_matches_mu_with_exposed_term_and_default_context():
    expr = Mu(
        ID("alpha", A),
        A,
        Mu(ID("_", A), A, Goal("1", A), ID("alt3", A)),
        Laog("L", A),
    )

    expander = _term_expander()

    assert expander._matches_bureaucratic_term_redex_mu(expr) is True



def test_bureaucratic_schema_2_matches_mutilde_with_default_term_and_exposed_context():
    expr = Mutilde(
        DI("beta", A),
        A,
        Goal("1", A),
        Mutilde(DI("_", A), A, DI("alt3", A), Laog("L", A)),
    )

    expander = _context_expander()

    assert expander._matches_bureaucratic_context_redex_mutilde(expr) is True



def test_bureaucratic_schema_3_matches_mutilde_with_exposed_term_and_default_context():
    expr = Mutilde(
        DI("beta", A),
        A,
        Mu(ID("_", A), A, Goal("1", A), ID("alt3", A)),
        Laog("L", A),
    )

    expander = _term_expander()

    assert expander._matches_bureaucratic_term_redex_mutilde(expr) is True



def test_bureaucratic_schema_4_matches_mu_with_default_term_and_exposed_context():
    expr = Mu(
        ID("alpha", A),
        A,
        Goal("1", A),
        Mutilde(DI("_", A), A, DI("alt3", A), Laog("L", A)),
    )

    expander = _context_expander()

    assert expander._matches_bureaucratic_context_redex_mu(expr) is True



def test_bureaucratic_case_1_blocks_under_affine_binder():
    candidate = Goal("1", A)
    parent = Mu(ID("_", A), A, candidate, ID("alt", A))

    expander = _term_expander()

    assert expander._would_introduce_bureaucratic_default_redex(candidate, parent) is True



def test_bureaucratic_case_2_blocks_inside_support_structure():
    candidate = Mu(ID("_", A), A, Goal("1", A), ID("alt", A))
    parent = Mu(ID("alt", A), A, candidate, Mutilde(DI("_", A), A, DI("supporter", A), ID("alt", A)))

    expander = _term_expander()

    assert expander._would_introduce_bureaucratic_default_redex(candidate, parent) is True



def test_bureaucratic_case_4_blocks_under_eta_long_expression():
    candidate = Mu(
        ID("alpha", A),
        A,
        Lamda(Hyp(DI("g", B), B), Goal("1", A)),
        ID("p2", A),
    )
    parent = Mu(ID("p2", A), A, candidate, ID("p2", A))

    expander = _term_expander()

    assert expander._would_introduce_bureaucratic_default_redex(candidate, parent) is True



def test_bureaucratic_case_3_allows_buried_leaf_under_lambda():
    candidate = Goal("1", A)
    parent = Lamda(Hyp(DI("g", B), B), candidate)

    expander = _term_expander()

    assert expander._would_introduce_bureaucratic_default_redex(candidate, parent) is False



def test_bureaucratic_case_6_allows_complex_expression_under_lambda():
    candidate = Mu(ID("alt3", A), A, Mu(ID("_", A), A, Goal("1", A), ID("alt3", A)), Laog("1", A))
    parent = Lamda(Hyp(DI("f", C), C), candidate)

    expander = _term_expander()

    assert expander._would_introduce_bureaucratic_default_redex(candidate, parent) is False