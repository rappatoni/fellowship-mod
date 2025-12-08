from conftest import load_monolith

def test_undercut_support():
    load_monolith().undercut_test()
