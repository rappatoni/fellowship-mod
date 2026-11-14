"""pytest plugin: on every adf-bdd call AIDA makes, also compute the stable
models with `naive --stm` (AIDA today) and `hybrid --stmng` (candidate),
compare them, and log one JSON line per distinct ADF. Results unchanged."""
import json, os, subprocess, tempfile, time, hashlib

LOG = os.environ.get("STMCHECK_LOG", "/tmp/stmcheck.jsonl")
TIMEOUT = float(os.environ.get("STMCHECK_TIMEOUT", "30"))
_seen = set()

def _run(binary, lib, mode, path):
    t = time.perf_counter()
    try:
        r = subprocess.run([binary, "-q", "--lx", "--lib", lib, mode, path],
                           capture_output=True, text=True, timeout=TIMEOUT)
        lines = sorted(l.strip() for l in r.stdout.splitlines() if l.strip())
        return {"t": round(time.perf_counter() - t, 4), "rc": r.returncode,
                "models": lines, "err": r.stderr.strip()[:200]}
    except subprocess.TimeoutExpired:
        return {"t": None, "rc": None, "models": None, "err": "timeout"}

def pytest_configure(config):
    from core.comp import oracle
    original = oracle.run_adf_bdd

    def wrapped(adf, mode="grounded", binary=None):
        if adf.statements:
            text, _ = oracle.diamond_export(adf)
            h = hashlib.sha1(text.encode()).hexdigest()
            if h not in _seen:
                _seen.add(h)
                b = binary or oracle.resolve_adf_bdd()
                with tempfile.NamedTemporaryFile("w", suffix=".adf", delete=False) as fh:
                    fh.write(text); path = fh.name
                try:
                    grd = _run(b, "naive", "--grd", path)
                    undec = sum(m.count("u(") for m in (grd["models"] or []))
                    old = _run(b, "naive", "--stm", path)
                    new = _run(b, "hybrid", "--stmng", path)
                    newgrd = _run(b, "hybrid", "--grd", path)
                finally:
                    os.unlink(path)
                rec = {"hash": h[:10], "n": len(adf.statements), "undec": undec,
                       "naive_grd": grd["t"], "hybrid_grd": newgrd["t"],
                       "naive_stm": old["t"], "hybrid_stmng": new["t"],
                       "n_models_old": None if old["models"] is None else len(old["models"]),
                       "n_models_new": None if new["models"] is None else len(new["models"]),
                       "agree": (old["models"] == new["models"]) if old["models"] is not None and new["models"] is not None else None,
                       "grd_agree": grd["models"] == newgrd["models"],
                       "rc": [old["rc"], new["rc"]], "err": [old["err"], new["err"]][:2]}
                with open(LOG, "a") as fh:
                    fh.write(json.dumps(rec) + "\n")
        return original(adf, mode, binary)

    oracle.run_adf_bdd = wrapped
    import core.comp.adf_label as al
    if getattr(al, "run_adf_bdd", None) is original:
        al.run_adf_bdd = wrapped
