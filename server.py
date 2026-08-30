"""
AIDA backend — session-based version.

Change from the previous single-request server.py: a prover process and its
registered arguments now live for the lifetime of a *session* (one debate),
not just one request. This is required for /compile to work at all — you
can't attack/undercut/support an argument that no longer exists because its
prover process was torn down after the last request.

Endpoints:
  POST   /sessions                      -> create a debate session
  DELETE /sessions/{sid}                -> close it (kills the prover process)
  POST   /sessions/{sid}/declare        -> send raw 'declare ...' commands
  POST   /sessions/{sid}/arguments      -> register+typecheck one argument
  POST   /sessions/{sid}/compile        -> attack/undercut/rebut/support/
                                            undergird/reinforce two existing
                                            arguments into a new one
  GET    /sessions/{sid}/graph          -> nodes + edges, for the frontend

Not yet handled (flagged for Max, don't build around these until answered):
  - error format for the type checker (raw Fellowship text for now)
  - the actual debate-graph *compilation* semantics (DEB A / DEB B shape)
  - live vs manual recheck for the colored graph UI
"""

from __future__ import annotations

import uuid
from typing import Dict, List, Optional

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

from wrap.cli import setup_prover
from core.dc.argument import Argument
from core.comp.color import DebateTermLabeller

STATUS_MAP = {"green": "ACCEPTED", "red": "DEFEATED", "yellow": "OPEN"}

COMPILE_ACTIONS = {
    "attack": lambda a, b, **kw: a.attack(b, **kw),
    "undercut": lambda a, b, **kw: a.undercut(b, **kw),
    "rebut": lambda a, b, **kw: a.rebut(b, **kw),
    "support": lambda a, b, **kw: a.support(b, **kw),
    "undergird": lambda a, b, **kw: a.undergird(b, **kw),
    "reinforce": lambda a, b, **kw: a.reinforce(b, **kw),
}

# --- Session state -----------------------------------------------------
# One entry per active debate. Kept in memory only — restarting the server
# loses all sessions. Fine for a prototype; revisit if this needs to survive
# restarts.
class Session:
    def __init__(self):
        self.prover = setup_prover()
        self.arguments: Dict[str, Argument] = {}
        # edges: list of {"from": str, "to": str, "action": str, "result": str}
        self.edges: List[dict] = []

    def close(self):
        self.prover.close()


SESSIONS: Dict[str, Session] = {}


def get_session(sid: str) -> Session:
    session = SESSIONS.get(sid)
    if session is None:
        raise HTTPException(status_code=404, detail=f"No such session: {sid}")
    return session


def status_of(argument: Argument) -> str:
    if argument.normal_body is None:
        argument.normalize()
    raw = DebateTermLabeller().status_of(argument.normal_body)
    return STATUS_MAP.get(raw, raw.upper())


# --- Models --------------------------------------------------------------
class DeclareRequest(BaseModel):
    declarations: List[str]


class ArgumentRequest(BaseModel):
    name: str
    conclusion: str
    instructions: List[str]
    is_anti: bool = False


class ArgumentResponse(BaseModel):
    name: str
    conclusion: str
    status: str


class CompileRequest(BaseModel):
    action: str          # one of COMPILE_ACTIONS keys
    source: str          # name of the attacking/supporting argument
    target: str          # name of the argument being attacked/supported
    on: Optional[str] = None
    result_name: Optional[str] = None


class GraphNode(BaseModel):
    name: str
    conclusion: str
    status: str


class GraphEdge(BaseModel):
    source: str
    target: str
    action: str
    result: str


class GraphResponse(BaseModel):
    nodes: List[GraphNode]
    edges: List[GraphEdge]


# --- App -------------------------------------------------------------
app = FastAPI(title="AIDA Backend")
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.post("/sessions")
def create_session():
    sid = uuid.uuid4().hex[:8]
    session = Session()
    session.prover.send_command("lk.")
    SESSIONS[sid] = session
    return {"session_id": sid}


@app.delete("/sessions/{sid}")
def close_session(sid: str):
    session = get_session(sid)
    session.close()
    del SESSIONS[sid]
    return {"closed": sid}


@app.post("/sessions/{sid}/declare")
def declare(sid: str, req: DeclareRequest):
    session = get_session(sid)
    for decl in req.declarations:
        try:
            session.prover.send_command(decl)
        except Exception as e:
            # Type-checker behaviour: report which declaration failed, don't
            # just die. Raw Fellowship error for now — revisit once Max
            # answers whether this needs translating for the editor UI.
            raise HTTPException(
                status_code=400,
                detail={"failed_declaration": decl, "error": str(e)},
            )
    return {"declared": req.declarations}


@app.post("/sessions/{sid}/arguments", response_model=ArgumentResponse)
def register_argument(sid: str, req: ArgumentRequest):
    session = get_session(sid)
    if req.name in session.arguments:
        raise HTTPException(status_code=409, detail=f"Argument '{req.name}' already exists in this session")

    arg = Argument(
        session.prover,
        name=req.name,
        conclusion=req.conclusion,
        instructions=req.instructions,
        is_anti=req.is_anti,
    )
    try:
        arg.execute()
    except Exception as e:
        # This IS the type checker: instructions went in, Fellowship
        # rejected them, report back instead of crashing the session.
        raise HTTPException(
            status_code=400,
            detail={"argument": req.name, "instructions": req.instructions, "error": str(e)},
        )

    session.prover.register_argument(arg)
    session.arguments[req.name] = arg
    return ArgumentResponse(name=arg.name, conclusion=arg.conclusion, status=status_of(arg))


@app.post("/sessions/{sid}/compile", response_model=ArgumentResponse)
def compile_arguments(sid: str, req: CompileRequest):
    session = get_session(sid)

    if req.action not in COMPILE_ACTIONS:
        raise HTTPException(
            status_code=400,
            detail=f"Unknown action '{req.action}'. Must be one of: {list(COMPILE_ACTIONS)}",
        )
    if req.source not in session.arguments:
        raise HTTPException(status_code=404, detail=f"No argument named '{req.source}' in this session")
    if req.target not in session.arguments:
        raise HTTPException(status_code=404, detail=f"No argument named '{req.target}' in this session")

    source = session.arguments[req.source]
    target = session.arguments[req.target]
    result_name = req.result_name or f"{req.source}_{req.action}_{req.target}"

    try:
        kwargs = {"name": result_name}
        if req.on is not None:
            kwargs["on"] = req.on
        result = COMPILE_ACTIONS[req.action](source, target, **kwargs)
    except Exception as e:
        raise HTTPException(
            status_code=400,
            detail={"action": req.action, "source": req.source, "target": req.target, "error": str(e)},
        )

    session.prover.register_argument(result)
    session.arguments[result.name] = result
    session.edges.append({
        "from": req.source,
        "to": req.target,
        "action": req.action,
        "result": result.name,
    })

    return ArgumentResponse(name=result.name, conclusion=result.conclusion, status=status_of(result))


@app.get("/sessions/{sid}/graph", response_model=GraphResponse)
def get_graph(sid: str):
    session = get_session(sid)
    nodes = [
        GraphNode(name=arg.name, conclusion=arg.conclusion, status=status_of(arg))
        for arg in session.arguments.values()
    ]
    edges = [
        GraphEdge(source=e["from"], target=e["to"], action=e["action"], result=e["result"])
        for e in session.edges
    ]
    return GraphResponse(nodes=nodes, edges=edges)