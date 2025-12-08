from conftest import load_monolith

def test_machine_integration():
    load_monolith().machine_integration_test()
