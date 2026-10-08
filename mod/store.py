from typing import Dict, Any

arguments: Dict[str, Any] = {}
#: The document graph (one per session, like ``arguments``): key "graph"
#: holds a core.dc.debate_graph.DebateGraph built from every registered
#: atomic argument (Phase C, aida-document-graph).
document: Dict[str, Any] = {}
