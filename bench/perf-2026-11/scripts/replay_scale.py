"""Fellowship replay (the type check) against term size.

    ACDC_FSP=bench/perf-2026-11/builds/fsp-native-pathcopy FSP_TYPECHECK=0 \
      FSP_MACHINE=1 ACDC_TIMEOUT=1200 .venv/bin/python bench/perf-2026-11/scripts/replay_scale.py 5 7 8 9

For each k, a document of k "double" steps (P_i derived twice from
P_{i-1}) whose expanded issue term doubles with k; the issue term of P_k
is replayed through Fellowship (core/dc/typecheck.typecheck) and timed:
total, the quiet replay (send_commands_quiet_final), its steps, the parse
of the term Fellowship sends back, the per-step round trips at the start,
middle and end, and the largest replies. PX_FIX=1 sets pexpect's maxread
to 1 MiB and searchwindowsize to 512 (the wrapper default: 2000 / None).
"""
import os, sys, time, logging
logging.disable(logging.WARNING)
from wrap.cli import setup_prover
from wrap.service import Service
import wrap.prover as wp
import core.ac.syntax as syn


def doc(k):
    L = ["lk.", "declare " + ", ".join(f"P{i}" for i in range(k + 1)) + " : bool."]
    for i in range(1, k + 1):
        for a in range(2):
            L.append(f"declare r{i}_{a} : (P{i-1} -> P{i}).")
    L += ["argument a0 : (P0).", "by default.", "dixi."]
    for i in range(1, k + 1):
        for a in range(2):
            L += [f"argument a{i}_{a} : (P{i}).", f"cut (P{i-1} -> P{i}) th.", f"axiom r{i}_{a}.",
                  "elim.", "next.", "axiom.", "dixi."]
    return "\n".join(L) + "\n"


def size(n):
    cnt, stack = 0, [n]
    while stack:
        x = stack.pop(); cnt += 1
        for v in (vars(x).values() if hasattr(x, "__dict__") else []):
            if isinstance(v, list):
                stack.extend(c for c in v if hasattr(c, "__dict__") and type(c).__module__.startswith("core.ac"))
            elif hasattr(v, "__dict__") and type(v).__module__.startswith("core.ac"):
                stack.append(v)
    return cnt


T = {"fsp": 0.0, "parse": 0.0, "steps": 0}
_quiet = wp.ProverWrapper.send_commands_quiet_final
def quiet(self, commands, silent=1):
    t = time.perf_counter(); r = _quiet(self, commands, silent)
    T["fsp"] += time.perf_counter() - t; T["steps"] = len(commands); return r
wp.ProverWrapper.send_commands_quiet_final = quiet
_ppt = syn.parse_proof_term
def ppt(*a, **kw):
    t = time.perf_counter(); r = _ppt(*a, **kw); T["parse"] += time.perf_counter() - t; return r
syn.parse_proof_term = ppt
import core.ac.grammar as gr
if hasattr(gr, "parse_proof_term"):
    gr.parse_proof_term = ppt
REC = []
def send(self, command, *a, **kw):
    t0 = time.perf_counter()
    self.prover.sendline(command); self.prover.expect(wp.REPLY_END)
    REC.append((command[:40], len(self.prover.before or ""), time.perf_counter() - t0))
    return self._finalize_state_from_output(self._reply(), command_for_error=command,
        silent=kw.get("silent", 1), include_ui=kw.get("include_ui", False),
        allow_incomplete=kw.get("allow_incomplete", False))

for k in [int(a) for a in sys.argv[1:]]:
    p = setup_prover(); svc = Service.of(p); svc.check(doc(k)); s = svc.session
    if os.environ.get("PX_FIX"):
        p.prover.maxread = 1 << 20; p.prover.searchwindowsize = 512
    found = svc.issue_term(svc.target(f"issue :P{k}"), want_term=False)
    term = s.issue_term(found.issue)
    from core.dc.typecheck import typecheck
    T.update(fsp=0.0, parse=0.0, steps=0); REC.clear()
    orig = wp.ProverWrapper._send; wp.ProverWrapper._send = send
    t = time.perf_counter(); typecheck(s, term, f"tc{k}", f"P{k}", False); total = time.perf_counter() - t
    wp.ProverWrapper._send = orig
    n = len(REC); seg = lambda lo, hi: 1e6 * sum(r[2] for r in REC[lo:hi]) / max(hi - lo, 1)
    print(f"k={k:2d} nodes={size(term):7d} steps={T['steps']:6d} total={total:7.2f}s fsp={T['fsp']:7.2f}s "
          f"({1e6*T['fsp']/max(T['steps'],1):6.0f} us/step) parse_back={T['parse']:6.2f}s "
          f"round trip us start/middle/end={seg(0,n//10):.0f}/{seg(n//2-n//20,n//2+n//20):.0f}/{seg(n-n//10,n):.0f}", flush=True)
    for c, b, dt in sorted(REC, key=lambda r: -r[1])[:2]:
        print(f"      largest reply: {c!r} {b} chars {dt:.2f}s", flush=True)
    p.close()
