import pytest
from conftest import load_monolith

@pytest.mark.parametrize("reduce", ["false"])
def test_focussed_undercut(reduce):
    load_monolith().focussed_undercut_test(reduce=reduce)
