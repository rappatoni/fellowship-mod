from core.ac.ast import Mu, Mutilde, Lamda, Admal, Goal, Laog, ID, DI, Hyp, Pyh
from core.comp.alpha import FreshenBinderNames


class FakePyh(Pyh):
    def __init__(self, id_node):
        self.id = id_node
        self.pres = None
        self.flag = None


def test_freshen_binder_names_preserves_affine_underscore_for_mu_like_binders():
    mu = Mu(ID("_", "P"), "P", Goal("lhs", "P"), Laog("rhs", "P"))
    mutilde = Mutilde(DI("_", "P"), "P", Goal("lhs", "P"), Laog("rhs", "P"))

    freshener = FreshenBinderNames(taken={"_"})
    mu_out = freshener.visit(mu)
    mutilde_out = freshener.visit(mutilde)

    assert mu_out.id.name == "_"
    assert mutilde_out.di.name == "_"


def test_freshen_binder_names_preserves_affine_underscore_for_lambda_like_binders():
    lam = Lamda(Hyp(DI("_", "P"), "P"), Goal("lhs", "P"))
    admal = Admal(FakePyh(ID("_", "P")), Laog("rhs", "P"))

    freshener = FreshenBinderNames(taken={"_"})
    lam_out = freshener.visit(lam)
    admal_out = freshener.visit(admal)

    assert lam_out.di.di.name == "_"
    assert admal_out.id.id.name == "_"
