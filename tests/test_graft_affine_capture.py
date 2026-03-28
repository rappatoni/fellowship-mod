import pytest

from wrap.cli import setup_prover
from wrap.prover import ProverError
from core.dc.argument import Argument


def test_affine_graft_bindings_do_not_capture_supported_defaults():
    prover = setup_prover()
    try:
        prover.send_command('lk.')
        prover.send_command('declare P,Q : bool.')

        p1 = Argument(prover, 'p1', '((P -> Q)->P)->P', [])
        p1.execute()
        s1 = Argument(prover, 's1', '((P -> Q)->P)->P', ['elim f'])
        s1.execute()
        d1 = s1.support(p1, name='d1')

        p2 = Argument(prover, 'p2', 'P', ['cut ((P -> Q)->P) alpha', 'elim g'])
        p2.execute()

        with pytest.raises(ProverError, match='This is not trivial'):
            p2.support(d1, name='d2')
    finally:
        prover.close()
