import pytest
from conftest import load_monolith

@pytest.mark.skip(reason="interactive mode requires stdin")
def test_interactive_mode():
    load_monolith().interactive_mode_test()
