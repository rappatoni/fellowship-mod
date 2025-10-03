import pytest
from conftest import load_monolith

@pytest.mark.xfail(reason="self-attack labeling incomplete/flaky")
def test_self_attack():
    load_monolith().self_attack_test()
