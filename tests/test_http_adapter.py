"""The reference HTTP adapter (examples/http_adapter.py): two sessions at
once stay independent, and every response is a valid envelope."""

import importlib.util
import json
import threading
import urllib.request
from pathlib import Path

import jsonschema
import pytest

SCHEMA = json.loads(Path("wrap/schemas/aida.schema.json").read_text())
spec = importlib.util.spec_from_file_location("http_adapter", "examples/http_adapter.py")
adapter = importlib.util.module_from_spec(spec)
spec.loader.exec_module(adapter)


@pytest.fixture
def base():
    server = adapter.serve(0)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    yield f"http://127.0.0.1:{server.server_address[1]}"
    server.shutdown()
    for session in list(adapter.SESSIONS.values()):
        session.close()
    adapter.SESSIONS.clear()


def call(url, method="GET", body=None):
    data = None if body is None else json.dumps(body).encode()
    request = urllib.request.Request(url, data=data, method=method,
                                     headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(request) as response:
            return response.status, json.loads(response.read())
    except urllib.error.HTTPError as e:
        return e.code, json.loads(e.read())


def test_two_sessions_check_their_own_documents_at_once(base):
    _, a = call(f"{base}/sessions", "POST")
    _, b = call(f"{base}/sessions", "POST")
    texts = {a["session"]: Path("tests/debates.fspy").read_text() + "evaluate tweety.\n",
             b["session"]: Path("tests/demo/05_even_loop.fspy").read_text()}
    results = {}

    def check(sid):
        results[sid] = call(f"{base}/sessions/{sid}/check", "POST", {"text": texts[sid]})

    threads = [threading.Thread(target=check, args=(sid,)) for sid in texts]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    for sid, (status, out) in results.items():
        assert status == 200
        jsonschema.validate(out, SCHEMA)
        assert out["kind"] == "report"
    _, inv_a = call(f"{base}/sessions/{a['session']}/inventory")
    _, inv_b = call(f"{base}/sessions/{b['session']}/inventory")
    names_a = {x["name"] for x in inv_a["arguments"]}
    names_b = {x["name"] for x in inv_b["arguments"]}
    assert "tweety" in names_a and "tweety" not in names_b and "Pdefault" in names_b
    status, ev = call(f"{base}/sessions/{a['session']}/query", "POST",
                      {"op": "evaluate", "target": "tweety"})
    assert status == 200 and ev["verdict"] == "value"
    jsonschema.validate(ev, SCHEMA)


def test_errors_are_envelopes(base):
    status, out = call(f"{base}/sessions/nope/inventory")
    assert status == 404 and out["kind"] == "error" and out["code"] == "not_found"
    _, s = call(f"{base}/sessions", "POST")
    status, out = call(f"{base}/sessions/{s['session']}/query", "POST", {"op": "evaluate", "target": "nobody"})
    assert status == 404 and out["code"] == "not_found"
    jsonschema.validate(out, SCHEMA)
    status, out = call(f"{base}/sessions/{s['session']}", "DELETE")
    assert status == 200


def test_registered_debates_over_http(base):
    """Register a debate by its term, extend it, add to the extension and
    close it - every response a valid envelope."""
    import sys
    sys.path.insert(0, "tests")
    from test_registered_debate import block
    from wrap.syntax import Command, parse_units
    units = [u for u in parse_units(block("tests/import/simple.fspy", "simple")) if isinstance(u, Command)]
    opened = next(i for i, u in enumerate(units) if u.kind == "register_debate")
    shared = " ".join(u.text + "." for u in units[:opened])
    own = [u.text + "." for u in units[opened + 1:] if u.kind == "opaque"]
    term = next(u["proof_term"] for u in units if u.kind == "debate_term")

    _, created = call(f"{base}/sessions", "POST", {})
    sid = created["session"]
    url = f"{base}/sessions/{sid}"

    def post(path, body):
        status, out = call(url + path, "POST", body)
        jsonschema.validate(out, SCHEMA)
        return status, out

    assert post("/check", {"text": shared})[0] == 200
    status, out = post("/debates", {"name": "simple", "issue": "A", "term": term,
                                    "declarations": own, "legacy": True})
    assert status == 200 and out["kind"] == "debate" and len(out["debate"]["arguments"]) == 5
    status, out = post("/debates/simple/extend", {"name": "s3"})
    assert status == 200 and out["debate"]["kind"] == "extension" and not out["debate"]["finished"]
    assert post("/debates/s3/declarations", {"text": "deny nb : (B)."})[0] == 200
    assert post("/debates/s3/arguments", {"name": "nb_arg", "conclusion": "B",
                                          "term": "μ'x:B.<x||nb>"})[0] == 200
    assert post("/debates/s3/moves", {"argument": "nb_arg", "verb": "attack",
                                      "target": "simple__a5"})[0] == 200
    status, out = post("/debates/s3/close", {})
    assert status == 200 and out["debate"]["finished"] and out["debate"]["parent"] == "simple"
    _, verdict = post("/query", {"op": "evaluate", "target": "s3"})
    _, parent = post("/query", {"op": "evaluate", "target": "simple"})
    assert verdict["verdict"] != "value" and parent["verdict"] == "value"
    status, out = post("/debates/simple/moves", {"argument": "x"})
    assert status == 422 and "not being recorded" in out["message"]
