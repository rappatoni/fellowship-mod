import pytest
from conftest import load_monolith

@pytest.mark.xfail(reason="pop_arg not fully implemented/stable")
def test_pop_subargument():
    load_monolith().pop_subargument_test()
