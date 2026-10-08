"""The document: everything a session holds that a new document replaces
(tasks.org, aida-sessions-and-documents).

A session (``wrap.prover.ProverWrapper``) owns one Fellowship process, its
settings and a lock; it holds one Document at a time.  ``new document``
(``ProverWrapper.new_document``) resets Fellowship with ``discard all.``,
sets the logic and swaps in a fresh Document; ``load FILE`` does the same
before running the file.  Nothing here is module-global, so two sessions in
one process never share a document.

The logic is chosen when a document starts.  Fellowship accepts ``lk.``,
``lj.``, ``minimal.`` and ``full.`` only before the first instruction;
``head`` records that no instruction has reached it yet.
"""

from typing import Any, Dict, Optional

from core.dc.debate_graph import DebateGraph

LOGICS = ("lk", "lj")


class Document:
    def __init__(self, logic: str = "lk", minimal: bool = False, revision: int = 0):
        if logic not in LOGICS:
            raise ValueError(f"unknown logic '{logic}' (expected one of {', '.join(LOGICS)})")
        #: "lk" (classical) or "lj" (intuitionistic), and whether minimal
        #: (no ex falso): the four logics Fellowship offers.
        self.logic = logic
        self.minimal = minimal
        #: No instruction has reached Fellowship yet: a logic toggle is
        #: still allowed.
        self.head = True
        #: Strict names Fellowship holds: sorts, axioms, theorems, moxias.
        self.declarations: Dict[str, Any] = {}
        #: Natural-language decorations of names (pres/decorations.py).
        self.decorations: Dict[str, str] = {}
        #: Every name taken in the document, with what took it.
        self.names: Dict[str, str] = {}
        #: Registered arguments, in registration order (the order unfolding
        #: nests scaffolds in).
        self.arguments: Dict[str, Any] = {}
        #: The document graph: every registered atomic argument's edges.
        self.graph = DebateGraph()
        #: Recorded debates (core/dc/debate.py) and the one being recorded.
        self.debates: Dict[str, Any] = {}
        self.recording_debate: Optional[str] = None
        #: ``anon_k`` names of shared sub-debates (core/dc/share.py).
        self.anon: Dict[Any, str] = {}
        #: Changes whenever the document does; terms cached at an older
        #: revision are stale.  Monotonic across documents of one session.
        self.revision = revision
        #: Caches, all invalidated by the revision or by a new document.
        self.issue_terms: Dict[Any, Any] = {}
        self.typechecked: Dict[Any, Any] = {}
        self.strict_edges: Dict[Any, Any] = {}

    @property
    def logic_name(self) -> str:
        """The logic as Fellowship's commands spell it: ``minimal lk`` etc."""
        return f"minimal {self.logic}" if self.minimal else self.logic
