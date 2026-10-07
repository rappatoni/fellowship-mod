"""aida-trailing-next-warning: the instruction generator never ends with a
`next`, so a support whose adapter leaves its open site last no longer
trips the "impossible to switch" special case in Argument.execute."""

from core.ac.ast import Mu, Mutilde, Goal, Laog, ID, DI
from core.ac.instructions import InstructionsGenerationVisitor


def gen(node, root="thesis"):
    return list(InstructionsGenerationVisitor(root_name=root).return_instructions(node))


def test_open_site_as_last_goal_emits_no_trailing_next():
    # mu thesis:A.< ?1:A || thesis >  - the adapter shape support builds.
    # The only instruction the site would produce is a `next` with nothing
    # after it, so the replay is empty: the goal simply stays open.
    body = Mu(ID("thesis", "A"), "A", Goal("1", "A"), ID("thesis", "A"))
    assert gen(body) == []


def test_next_between_two_open_sites_is_kept_and_the_final_one_dropped():
    root = Mu(ID("thesis", "P"), "P", Goal("1", "P"), Laog("2", "P"))
    root.contr = "P"
    assert gen(root) == ["next"]


def test_cut_root_keeps_its_inner_next_only():
    root = Mu(ID("demo", "P"), "P", Goal("1", "P"), Laog("2", "P"))
    root.contr = "P"
    assert gen(root, root="demo") == ["cut (P) demo", "next"]


def test_laog_site_last_is_also_stripped():
    body = Mutilde(DI("thesis", "A"), "A", DI("x", "A"), Laog("1", "A"))
    instructions = gen(body)
    assert not instructions or instructions[-1] != "next"
