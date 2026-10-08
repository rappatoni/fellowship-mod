from wrap.cli import setup_prover

def test_machine_stub():
    pw = setup_prover()   # resolves fsp as the CLI does and forces machine mode
    state = pw.send_command('lj.')
    assert state.get('mode') in {None, 'idle', 'success', 'subgoals', 'exception'}
    pw.close()
