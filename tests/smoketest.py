import os
from wrap.prover import ProverWrapper

def test_machine_stub(monkeypatch):
    monkeypatch.setenv("FSP_HUMAN_UI", "0")  # enforce machine mode
    pw = ProverWrapper('./fsp')
    state = pw.send_command('lj.')
    assert state.get('mode') in {None, 'idle', 'success', 'subgoals', 'exception'}
    pw.close()
