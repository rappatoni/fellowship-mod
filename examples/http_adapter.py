"""A minimal HTTP adapter over AIDA's service layer - a reference for the
document UI's server, not a product (docs/dui-integration.md).

Standard library only.  One AIDA session (one Fellowship process) per
client session, kept in memory; every request runs under that session's
lock (wrap/service.py).  Run it with

    .venv/bin/python examples/http_adapter.py [PORT]

Endpoints (JSON in, JSON out; every response is a versioned envelope of
wrap/serialize.py, described by wrap/schemas/aida.schema.json):

    POST   /sessions                      -> {"session": ID}
    DELETE /sessions/ID                   -> {"closed": ID}
    POST   /sessions/ID/check   {"text"}  -> a "report": one entry per command
    POST   /sessions/ID/import  {"language", "data", "name"?}  -> an "import"
    POST   /sessions/ID/debates {"name", "issue", "term", "declarations"?, "legacy"?}
                                          -> a "debate", registered by its term
    POST   /sessions/ID/debates {"name", "language", "source" | "data"}
                                          -> a "debate", imported into the document
    POST   /sessions/ID/debates/NAME/extend {"name"}   -> a "debate" extending NAME,
                                          being recorded until .../close
    POST   /sessions/ID/debates/NAME/moves {"argument", "verb"?, "target"?}
    POST   /sessions/ID/debates/NAME/arguments {"name", "conclusion", "term"}
    POST   /sessions/ID/debates/NAME/declarations {"text"}
    POST   /sessions/ID/debates/NAME/close    -> "done": the debate being recorded
    GET    /sessions/ID/inventory         -> an "inventory"
    POST   /sessions/ID/query   {"op": "graph"|"label"|"evaluate"|"term"|"render"|"tree",
                                 "target": "NAME" | "issue :X", ...options}
                                          -> a "graph", "labellings", "evaluation", ...

A query works on the document the last check or import left in the
session.  A refusal is an "error" envelope with HTTP status 422 (404 for
an unknown session or target).
"""

from __future__ import annotations

import json
import sys
import threading
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from wrap.cli import setup_prover            # noqa: E402
from wrap.service import AidaError, InvalidRequest, NotFound, Service  # noqa: E402
from wrap.serialize import envelope, to_json  # noqa: E402

SESSIONS: dict = {}
_REGISTRY_LOCK = threading.Lock()

_QUERIES = {
    "graph": lambda s, t, o: s.graph(t, whole=o.get("whole", False), labels=True),
    "label": lambda s, t, o: s.label(t, o.get("semantics", "grounded")),
    "evaluate": lambda s, t, o: s.evaluate(t, mode=o.get("mode", "skeptical"),
                                           semantics=o.get("semantics", "preferred"),
                                           base=o.get("base", "cbn"), witness=o.get("witness"),
                                           favour=o.get("favour", False)),
    "term": lambda s, t, o: s.term(t, o.get("which")),
    "render": lambda s, t, o: s.render(t, o.get("which"), o.get("style")),
    "tree": lambda s, t, o: s.tree(t, mode=o.get("mode", "pt"), which=o.get("which")),
}


class Handler(BaseHTTPRequestHandler):
    def _send(self, status: int, body: dict) -> None:
        data = json.dumps(body, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(data)

    def _body(self) -> dict:
        length = int(self.headers.get("Content-Length") or 0)
        return json.loads(self.rfile.read(length) or b"{}")

    def _session(self, sid: str):
        with _REGISTRY_LOCK:
            session = SESSIONS.get(sid)
        if session is None:
            raise NotFound(f"no session '{sid}'")
        return session

    def _dispatch(self, method: str) -> None:
        parts = [p for p in self.path.split("?")[0].split("/") if p]
        try:
            if parts == ["sessions"] and method == "POST":
                sid = uuid.uuid4().hex[:12]
                session = setup_prover()
                with _REGISTRY_LOCK:
                    SESSIONS[sid] = session
                return self._send(201, {"session": sid})
            if len(parts) < 2 or parts[0] != "sessions":
                return self._send(404, envelope("error", code="not_found", stage=None,
                                                message=f"no route {self.path}", span=None,
                                                cause=None, diagnostics=[]))
            sid, rest = parts[1], parts[2:]
            if method == "DELETE" and not rest:
                session = self._session(sid)
                with _REGISTRY_LOCK:
                    SESSIONS.pop(sid, None)
                session.close()
                return self._send(200, {"closed": sid})
            session = self._session(sid)
            service = Service.of(session)
            if rest == ["check"] and method == "POST":
                result = service.check(self._body()["text"])
            elif rest == ["import"] and method == "POST":
                body = self._body()
                result = service.import_document(body["language"], body["data"],
                                                 name=body.get("name", "imported"))
            elif rest == ["inventory"] and method == "GET":
                result = service.inventory()
            elif rest[:1] == ["debates"] and method == "POST":
                result = self._debates(session, service, rest[1:], self._body())
            elif rest == ["query"] and method == "POST":
                body = self._body()
                op = _QUERIES.get(body.get("op"))
                if op is None:
                    return self._send(400, envelope("error", code="invalid", stage=None,
                                                    message=f"unknown op {body.get('op')!r}",
                                                    span=None, cause=None, diagnostics=[]))
                result = op(service, service.target(body["target"]), body)
            else:
                return self._send(404, envelope("error", code="not_found", stage=None,
                                                message=f"no route {method} {self.path}",
                                                span=None, cause=None, diagnostics=[]))
            return self._send(200, to_json(result, session))
        except NotFound as e:
            return self._send(404, to_json(e))
        except AidaError as e:
            return self._send(422, to_json(e))
        except (KeyError, ValueError) as e:
            return self._send(400, envelope("error", code="invalid", stage=None, message=str(e),
                                            span=None, cause=None, diagnostics=[]))

    @staticmethod
    def _debates(session, service, rest, body):
        """The debate routes: register or import one, extend one, and add
        to the one being recorded."""
        if not rest:
            if "language" in body:
                return service.import_debate(body["name"], body["language"], body.get("source"),
                                             data=body.get("data"))
            return service.register_debate(body["name"], body["issue"], body["term"],
                                           body.get("declarations", []), legacy=body.get("legacy", False))
        name, action = rest[0], rest[1:]
        if action == ["extend"]:
            return service.start_extension(body["name"], name)
        if session.recording_debate != name:
            raise InvalidRequest(f"debate '{name}' is not being recorded.", stage="debate")
        if action == ["moves"]:
            return service.move(body["argument"], body.get("verb"), body.get("target"))
        if action == ["arguments"]:
            return service.register(body["name"], body["conclusion"], body["term"])
        if action == ["declarations"]:
            return service.command(body["text"])
        if action == ["close"]:
            return service.close_debate()
        raise NotFound(f"no debate route {'/'.join(action)}")

    def do_POST(self):
        self._dispatch("POST")

    def do_GET(self):
        self._dispatch("GET")

    def do_DELETE(self):
        self._dispatch("DELETE")

    def log_message(self, fmt, *args):         # quiet by default
        pass


def serve(port: int = 8000) -> ThreadingHTTPServer:
    return ThreadingHTTPServer(("127.0.0.1", port), Handler)


if __name__ == "__main__":
    server = serve(int(sys.argv[1]) if len(sys.argv) > 1 else 8000)
    print(f"AIDA reference adapter on http://127.0.0.1:{server.server_address[1]}")
    try:
        server.serve_forever()
    finally:
        for session in SESSIONS.values():
            session.close()
