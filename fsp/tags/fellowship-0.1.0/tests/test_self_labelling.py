import pytest
from conftest import load_monolith

@pytest.mark.skip(reason="requires an argument instance via eval()")
def test_self_labelling():
    load_monolith().self_labelling_test()
