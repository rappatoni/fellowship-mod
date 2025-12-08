from copy import deepcopy
from typing import Optional
from pres.color import AcceptanceColoringVisitor
from pres.gen import ProofTermGenerationVisitor
from pres.nl import (
    natural_language_rendering,
    natural_language_dialectical_rendering,
    natural_language_argumentative_rendering,
)
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI

class AcceptanceTreeRenderer:
    _gv_colors = {"green": "palegreen2", "red": "lightcoral", "yellow": "khaki1"}

    def __init__(self, verbose: bool = False, *, label_mode: str = "proof", nl_style: str = "argumentation"):
        self.color_v = AcceptanceColoringVisitor(verbose=verbose)
        self._idmap: dict[int, str] = {}
        self._seq = 0
        self.label_mode = label_mode  # "proof" or "nl"
        if nl_style == "dialectical":
            self._nl_sem = natural_language_dialectical_rendering
        elif nl_style == "intuitionistic":
            self._nl_sem = natural_language_rendering
        else:
            self._nl_sem = natural_language_argumentative_rendering

    def _nid(self, n) -> str:
        k = id(n)
        v = self._idmap.get(k)
        if v is None:
            self._seq += 1
            v = f"n{self._seq}"
            self._idmap[k] = v
        return v

    def _label_leaf(self, n) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))

    def _class_or_inherit(self, n, parent_color: Optional[str]) -> Optional[str]:
        try:
            return self.color_v.classify(n)
        except Exception:
            return parent_color

    def _label_and_child(self, n) -> tuple[str, Optional[ProofTerm]]:
        found = {"child": None}
        def pp(node) -> str:
            if isinstance(node, Mu):
                if found["child"] is None and isinstance(node.term, Goal):
                    goal = ProofTermGenerationVisitor().visit(deepcopy(node.term)).pres
                    found["child"] = node.context
                    return f"μ{node.id.name}:{node.prop}.<{goal}||…>"
                if found["child"] is None and isinstance(node.context, Laog):
                    laog = ProofTermGenerationVisitor().visit(deepcopy(node.context)).pres
                    found["child"] = node.term
                    return f"μ{node.id.name}:{node.prop}.<…||{laog}>"
                return f"μ{node.id.name}:{node.prop}.<{pp(node.term)}||{pp(node.context)}>"
            if isinstance(node, Mutilde):
                if found["child"] is None and isinstance(node.term, Goal):
                    goal = ProofTermGenerationVisitor().visit(deepcopy(node.term)).pres
                    found["child"] = node.context
                    return f"μ'{node.di.name}:{node.prop}.<{goal}||…>"
                if found["child"] is None and isinstance(node.context, Laog):
                    laog = ProofTermGenerationVisitor().visit(deepcopy(node.context)).pres
                    found["child"] = node.term
                    return f"μ'{node.di.name}:{node.prop}.<…||{laog}>"
                return f"μ'{node.di.name}:{node.prop}.<{pp(node.term)}||{pp(node.context)}>"
            if isinstance(node, Lamda):
                return f"λ{node.di.di.name}:{node.di.prop}.{pp(node.term)}"
            if isinstance(node, Cons):
                return f"{pp(node.term)}*{pp(node.context)}"
            if isinstance(node, Goal):
                g = deepcopy(node); g = ProofTermGenerationVisitor().visit(g); return g.pres
            if isinstance(node, Laog):
                l = deepcopy(node); l = ProofTermGenerationVisitor().visit(l); return l.pres
            if isinstance(node, ID):
                return f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
            if isinstance(node, DI):
                return f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
            return self._label_leaf(node)
        lbl = pp(n)
        return lbl, found["child"]

    def _nl_label_with_elision(self, n: ProofTerm, child: Optional[ProofTerm]) -> str:
        lines: list[str] = []
        sem = self._nl_sem
        def rec(node: ProofTerm, indent: int) -> None:
            if child is not None and node is child:
                lines.append(f"{sem.indentation * indent}...")
                return
            if isinstance(node, Mu):
                lines.append(f"{sem.indentation * indent}{sem.Mu[0]}{node.prop}")
                rec(node.term, indent + 1)
                rec(node.context, indent + 1)
                return
            if isinstance(node, Mutilde):
                lines.append(f"{sem.indentation * indent}{sem.Mutilde[0]}{node.prop}")
                rec(node.term, indent)
                lines.append(f"{sem.indentation * indent}{sem.Mutilde[1]}")
                rec(node.context, indent)
                return
            if isinstance(node, Lamda):
                lines.append(f"{sem.indentation * indent}{sem.Lamda}{node.di.prop}({node.di.di.name})")
                rec(node.term, indent)
                return
            if isinstance(node, Cons):
                lines.append(f"{sem.indentation * indent}{sem.Cons}")
                rec(node.term, indent)
                rec(node.context, indent)
                return
            if isinstance(node, Goal):
                lines.append(f"{sem.indentation * indent}{sem.Goal}{node.number}")
                return
            if isinstance(node, DI):
                lines.append(f"{sem.indentation * indent}{sem.DI}{node.name}")
                return
            if isinstance(node, ID):
                lines.append(f"{sem.indentation * indent}{sem.ID}")
                return
            c = deepcopy(node)
            c = ProofTermGenerationVisitor().visit(c)
            lines.append(f"{sem.indentation * indent}{getattr(c, 'pres', repr(c))}")
        rec(n, 0)
        return "\n".join(lines)

    def _emit(self, n, lines: list[str], parent: Optional[str], parent_color: Optional[str]) -> None:
        if self.label_mode == "proof":
            label, child = self._label_and_child(n)
        else:
            _lbl_proof, child = self._label_and_child(n)
            label = self._nl_label_with_elision(n, child)
        col  = self._class_or_inherit(n, parent_color)
        fill = self._gv_colors.get(col)
        my = self._nid(n)
        attr = f'shape=box,style=filled,fillcolor="{fill}"' if fill else 'shape=box'
        esc_label = label.replace("\\", "\\\\").replace("\"", "\\\"")
        lines.append(f'  {my} [label="{esc_label}", {attr}];')
        if parent:
            lines.append(f'  {my} -> {parent};')
        if child is not None:
            self._emit(child, lines, my, col)

    def to_dot(self, root: ProofTerm) -> str:
        lines: list[str] = []
        lines.append('digraph ProofTree {')
        lines.append('  graph [rankdir=BT, splines=polyline];')
        lines.append('  node  [fontname="Helvetica"];')
        lines.append('  edge  [fontname="Helvetica", color="orangered", penwidth=2, arrowhead="vee"];')
        self._emit(root, lines, parent=None, parent_color=None)
        lines.append('}')
        return "\n".join(lines)

def render_acceptance_tree_dot(pt: ProofTerm, verbose: bool = False, *, label_mode: str = "proof", nl_style: str = "argumentation") -> str:
    return AcceptanceTreeRenderer(verbose=verbose, label_mode=label_mode, nl_style=nl_style).to_dot(pt)
