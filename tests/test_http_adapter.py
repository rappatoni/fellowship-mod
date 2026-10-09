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
