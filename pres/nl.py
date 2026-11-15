import re
from typing import Mapping
from pres.decorations import render_fo_term, render_prop
from pres.pattern_render import (
    AlternativeCasesRenderer,
    AlternativeCounterexamplesRenderer,
    ApplicationRenderer,
    DefeasibleWarrantRenderer,
    DualApplicationRenderer,
    DualDefeasibleWarrantRenderer,
    PatternRenderContext,
    PatternRenderingRegistry,
)
from core.ac.ast import Deleg, ProofTerm, Mu, Mutilde, Lamda, Cons, Sonc, Admal, Goal, Laog, Deleg, Geled, ID, DI, LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO

def pretty_natural(
    proof_term: "ProofTerm",
    semantic: "Rendering_Semantics",
    *,
    declarations: Mapping[str, str] | None = None,
    decorations: Mapping[str, str] | None = None,
) -> str:
    lines = []
    traverse_proof_term(semantic, proof_term, lines, indent=0, declarations=declarations, decorations=decorations)
    if getattr(semantic, "tree_guides", False):
        lines = _with_tree_guides(lines, semantic.indentation)
    return '\n'.join(lines)


def _line_level(line: str, indentation: str) -> tuple[int, str]:
    level = 0
    rest = line
    while indentation and rest.startswith(indentation):
        level += 1
        rest = rest[len(indentation):]
    return level, rest


def _has_later_sibling(levels: list[int], index: int, level: int) -> bool:
    for later_level in levels[index + 1:]:
        if later_level < level:
            return False
        if later_level == level:
            return True
    return False


def _with_tree_guides(lines: list[str], indentation: str) -> list[str]:
    if not indentation:
        return lines
    parsed = [_line_level(line, indentation) for line in lines]
    levels = [level for level, _ in parsed]
    guided: list[str] = []
    for index, (level, text) in enumerate(parsed):
        if level == 0:
            guided.append(text)
            continue
        parts: list[str] = []
        for ancestor_level in range(1, level):
            parts.append("│  " if _has_later_sibling(levels, index, ancestor_level) else "   ")
        parts.append("├─ " if _has_later_sibling(levels, index, level) else "└─ ")
        guided.append("".join(parts) + text)
    return guided

class Rendering_Semantics:
    def __init__(self, indentation, Mu, Mutilde, Lamda, Cons, Goal, Laog, Deleg, Geled, ID, DI, *, pattern_renderers=None, connective_templates=None, tree_guides: bool = False,
                 LamdaFO_template="consider an arbitrary but fixed @var of type @sort",
                 DestructTermsPairFO_template="let @var of type @sort be such a thing",
                 ConsFO_template="instantiated at @witness",
                 TermsPairFO_template="witnessed by @witness",
                 ConsFO_anonymous_template=None,
                 binder_template=None,
                 owed_phrases=None,
                 cite_template="see @name: @prop",
                 shared_header_template="@name:",
                 shared_site_templates=None,
                 falsum_phrase=None):
        self.indentation = indentation
        self.Mu = Mu
        self.Mutilde = Mutilde
        self.Lamda = Lamda
        #self.Hyp = Hyp
        self.Cons = Cons
        self.Goal = Goal
        self.Laog = Laog
        self.Deleg = Deleg
        self.Geled = Geled
        #self.Done = Done
        self.ID = ID
        self.DI = DI
        self.connective_templates = dict(connective_templates or {})
        # Phrasing for the first-order nodes, keyword-only so the existing
        # positional constructions stay valid.  Defaults follow Fellowship's
        # own natural-language rendering (print.ml:63-129).
        self.LamdaFO = LamdaFO_template
        self.DestructTermsPairFO = DestructTermsPairFO_template
        self.ConsFO = ConsFO_template
        self.TermsPairFO = TermsPairFO_template
        # The instantiation line where the instantiated variable is unknown
        # (an unenriched term); None drops nothing from ConsFO_template, which
        # then simply has no @var.
        self.ConsFO_anonymous = ConsFO_anonymous_template
        # How a binder's name follows the proposition of its mu/mu~ line
        # (@binder), e.g. ". Starting check @binder:"; None writes " (name)".
        self.binder_template = binder_template
        self.tree_guides = tree_guides
        self.pattern_registry = PatternRenderingRegistry(pattern_renderers or [])
        # The phrase of an owed site by its label (tasks.org,
        # aida-render-text-styles): "pending" for a site of an unevaluated
        # term, "OUT" and "UNDEC" for the sites of a normal form (named by
        # their labels), "fallback" for the bottom alternative of a support
        # scaffold - the site that stands if no supporter does.  None keeps
        # the style's single Goal/Laog phrase.
        self.owed_phrases = dict(owed_phrases) if owed_phrases else None
        # A citation of a named sub-debate in a shared rendering
        # (``render ... shared``): @name and @prop.
        self.cite_template = cite_template
        # The shared rendering's header of a named sub-debate (@name), and
        # one line per open site it rests on: {"owed": ..., "presumed": ...},
        # each with @prop.
        self.shared_header_template = shared_header_template
        self.shared_site_templates = dict(shared_site_templates or {})
        # The falsum eliminator ``_F_`` (an empty proposition), where the
        # style has words for it.
        self.falsum_phrase = falsum_phrase

natural_language_rendering = Rendering_Semantics('   ', ["we need to prove ", "we proved ", ""], ["we proved ", ""], f"assume ", f"and", f"? ", f" ?", f" !", f"! ", f"done ", f"by ")
# The former `dialectical` style's phrases; only the acceptance tree's labels
# (pres/tree.py, `tree ARG nl dialectical`) still use them.  The `dialectical`
# render style is `dialectical_rendering` below.
natural_language_dialectical_rendering = Rendering_Semantics('   ', ["Assume a refutation of ", "Assume a proof of  ", ""], ["Assume a proof of  ", ""], f"assume ", f"and", f"? ", f" ?", f" !", f"! ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], ["We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"? ", f" ?", f"by default!", f"by default!", f"done ", f"by ", pattern_renderers=[AlternativeCasesRenderer(), AlternativeCounterexamplesRenderer(child_indent_delta=3), ApplicationRenderer(header_template="Für @prop ist hinreichend, dass @arg_prop", end_label=None), DualApplicationRenderer(header_template="Für @prop ist notwendig dass @condition_prop", warrant_indent_delta=3, end_label=None), DefeasibleWarrantRenderer(header_template="Für @prop spricht ", exception_indent_delta=3), DualDefeasibleWarrantRenderer(header_template="gegen @prop spricht ", support_indent_delta=3)])
def _checklist_semantics(*, mu, mutilde, lamda, cons, deleg, geled, id_, di, owed, patterns,
                         connectives, fo, cite, shared_header, shared_sites, falsum, binder):
    """A check-list style (German ``pruefschema``, English ``checklist``): the
    two share their structure and differ only in their phrases."""
    return Rendering_Semantics(
        '   ', [mu], [mutilde], lamda, cons, owed["pending"], owed["pending"], deleg, geled,
        list(id_), list(di),
        pattern_renderers=[
            AlternativeCasesRenderer(header_template=patterns["cases"],
                                     first_case_label=patterns["first_case"],
                                     next_case_label=patterns["next_case"]),
            AlternativeCounterexamplesRenderer(header_template=patterns["conditions"],
                                               first_condition_label=patterns["first_condition"],
                                               next_condition_label=patterns["next_condition"]),
            ApplicationRenderer(header_template=patterns["sufficient"], reason_label=patterns["because"],
                                separator_label=cons, end_label=patterns["completed"]),
            DualApplicationRenderer(header_template=patterns["necessary"], reason_label=patterns["because"],
                                    end_label=patterns["completed"]),
            DefeasibleWarrantRenderer(header_template=patterns["for"], exception_label=patterns["but"]),
            DualDefeasibleWarrantRenderer(header_template=patterns["against"],
                                          requirement_template=patterns["but"]),
        ],
        connective_templates=connectives,
        tree_guides=True,
        LamdaFO_template=fo["lamda"], DestructTermsPairFO_template=fo["destruct"],
        ConsFO_template=fo["cons"], TermsPairFO_template=fo["pair"],
        ConsFO_anonymous_template=fo["cons_anonymous"], binder_template=binder,
        owed_phrases=owed, cite_template=cite,
        shared_header_template=shared_header, shared_site_templates=shared_sites,
        falsum_phrase=falsum,
    )


# The German check-list ("Prüfschema").  An owed site is phrased by its label
# (aida-render-text-styles, decided with the author 2026-11-15): still to be
# checked in an unevaluated term, failed only where a normal form names it
# OUT, open where it names it UNDEC.  The end of a check names the result it
# hands back, not a verdict about the case.
pruefschema_rendering = _checklist_semantics(
    mu="Es genügt zu prüfen, dass ",
    mutilde="Es ist notwendigerweise zu prüfen, ob ",
    lamda="Angenommen ",
    cons="und",
    deleg="sofern nichts entgegensteht: ",
    geled="sofern nichts anderes bekannt ist, ist zu verneinen: ",
    id_=("es ist ausgeschlossen, dass: ", "Prüfung @binder abgeschlossen (bejaht)."),
    di=("es liegt vor: ", "Prüfung @binder abgeschlossen (verneint)."),
    binder=". Beginn der Prüfung @binder:",
    owed={"pending": "noch zu prüfen: ",
          "OUT": "Prüfung fehlgeschlagen: ",
          "UNDEC": "offen: ",
          "IN": "es liegt vor: ",
          "fallback": "andernfalls nicht erwiesen: "},
    patterns={
        "cases": "Die Prüfung @binder, ob @prop gilt, zerfällt in folgende Fallgruppen:",
        "first_case": "Fallgruppe @index:",
        "next_case": "oder Fallgruppe @index:",
        "conditions": "Zur Prüfung von @prop müssen folgende Prüfpunkte abgearbeitet werden:",
        "first_condition": "Prüfpunkt @index:",
        "next_condition": "und Prüfpunkt @index:",
        "sufficient": "Für @prop ist es hinreichend, dass @arg_prop gilt. Beginn der Prüfung @binder:",
        "necessary": "Für @prop ist notwendig, dass @condition_prop gilt. Beginn der Prüfung @binder:",
        "because": "weil",
        "completed": "Prüfung @binder abgeschlossen.",
        "for": "Für @prop spricht:",
        "against": "Gegen @prop spricht:",
        "but": "aber",
    },
    connectives={"->": "@left impliziert @right", "-": "@left ohne @right",
                 "~": "nicht @body",
                 "forall": "für alle @vars vom Typ @sort gilt: @body",
                 "exists": "es gibt ein @vars vom Typ @sort, so dass @body"},
    fo={"lamda": "für ein beliebiges, aber festes @var vom Typ @sort",
        "destruct": "sei @var vom Typ @sort ein solches",
        "cons": "eingesetzt für @var: @witness",
        "cons_anonymous": "angewandt auf @witness",
        "pair": "belegt durch @witness"},
    cite="siehe Prüfung @name: @prop",
    shared_header="Prüfung @name:",
    shared_sites={"owed": "zu zeigen: @prop", "presumed": "vermutet: @prop"},
    falsum="Widerspruch.",
)

# The English check-list: the Prüfschema's structure with English phrases
# (tasks.org, aida-render-text-styles).
checklist_rendering = _checklist_semantics(
    mu="It suffices to check that ",
    mutilde="It must be checked whether ",
    lamda="Assume ",
    cons="and",
    deleg="unless something speaks against it: ",
    geled="unless shown otherwise, deny: ",
    id_=("it is excluded that: ", "Check @binder completed (holds)."),
    di=("established: ", "Check @binder completed (refuted)."),
    binder=". Starting check @binder:",
    owed={"pending": "to be checked: ",
          "OUT": "not established: ",
          "UNDEC": "undecided: ",
          "IN": "established: ",
          "fallback": "otherwise not established: "},
    patterns={
        "cases": "The check @binder of whether @prop holds splits into the following cases:",
        "first_case": "Case @index:",
        "next_case": "or case @index:",
        "conditions": "To check @prop, the following points must be worked through:",
        "first_condition": "Point @index:",
        "next_condition": "and point @index:",
        "sufficient": "For @prop it suffices that @arg_prop holds. Starting check @binder:",
        "necessary": "For @prop it is necessary that @condition_prop holds. Starting check @binder:",
        "because": "because",
        "completed": "Check @binder completed.",
        "for": "In favour of @prop:",
        "against": "Against @prop:",
        "but": "but",
    },
    connectives={"->": "@left implies @right", "-": "@left without @right",
                 "~": "it is not the case that @body",
                 "forall": "for all @vars of type @sort, @body",
                 "exists": "for some @vars of type @sort, @body"},
    fo={"lamda": "for an arbitrary but fixed @var of type @sort",
        "destruct": "let @var of type @sort be such a thing",
        "cons": "instantiating @var with @witness",
        "cons_anonymous": "instantiated at @witness",
        "pair": "witnessed by @witness"},
    cite="see check @name: @prop",
    shared_header="Check @name:",
    shared_sites={"owed": "to be shown: @prop", "presumed": "presumed: @prop"},
    falsum="contradiction.",
)

# Vanilla rendering: preserves the full proof-term syntax, only adds indentation/line breaks.
# Implemented as a dedicated semantics object plus a visitor special-case.
vanilla_rendering = Rendering_Semantics('   ', "", "", "", "", "", "", "", "", "", "")

from core.comp.visitor import ProofTermVisitor

#: The binder of an alternative of a support or attack scaffold.
_ALT_BINDER = re.compile(r"alt\d+$")
from pres.gen import labelled_prop


class _VanillaVisitor(ProofTermVisitor):
    def __init__(self, semantic: Rendering_Semantics, lines: list[str], indent: int = 0, prefix: str = ""):
        super().__init__()
        self.semantic = semantic
        self.lines = lines
        self.indent = indent
        self.prefix = prefix or (self.semantic.indentation * indent)
        self._cur: list[str] = []

    def _emit(self, s: str) -> None:
        self._cur.append(s)

    def _newline(self) -> None:
        if self._cur:
            self.lines.append(self.prefix + "".join(self._cur))
            self._cur = []

    def _emit_line(self, s: str, prefix: str | None = None) -> None:
        line_prefix = self.prefix if prefix is None else prefix
        self.lines.append(line_prefix + s)

    @staticmethod
    def _child_prefix(parent_prefix: str, is_last: bool) -> str:
        return parent_prefix + ("└─ " if is_last else "├─ ")

    @staticmethod
    def _continuation_prefix(parent_prefix: str, is_last: bool) -> str:
        return parent_prefix + ("   " if is_last else "│  ")

    @staticmethod
    def _with_prefix(lines: list[str], first_prefix: str, rest_prefix: str) -> list[str]:
        if not lines:
            return []
        return [first_prefix + lines[0], *(rest_prefix + line for line in lines[1:])]

    def _render_child(self, node, *, is_last: bool) -> list[str]:
        return self._with_prefix(
            self._render_lines(node),
            self._child_prefix(self.prefix, is_last),
            self._continuation_prefix(self.prefix, is_last),
        )

    def _render_lines(self, node) -> list[str]:
        """``node`` rendered on its own, unprefixed, by this kind of visitor."""
        child_lines: list[str] = []
        type(self)(self.semantic, child_lines).render(node)
        return child_lines

    def render(self, node) -> None:
        self.visit(node)
        if self._cur:
            self._newline()

    def visit_Mu(self, node: Mu):
        opening_prefix = self.prefix
        self._emit(f"μ{node.id.name}:{labelled_prop(node.prop, node, False)}.<")
        self._newline()

        term_lines = self._render_child(node.term, is_last=False)
        context_lines = self._render_child(node.context, is_last=True)

        if term_lines:
            self.lines.extend(term_lines[:-1])
            last_term = term_lines[-1]
            self.lines.append(f"{last_term}||")
        else:
            self._emit_line("├─ ||", prefix=opening_prefix)
        self.lines.extend(context_lines)
        self._emit_line(">", prefix=opening_prefix)
        self._cur = []
        return node

    def visit_Mutilde(self, node: Mutilde):
        opening_prefix = self.prefix
        self._emit(f"μ'{node.di.name}:{labelled_prop(node.prop, node, True)}.<")
        self._newline()

        term_lines = self._render_child(node.term, is_last=False)
        context_lines = self._render_child(node.context, is_last=True)

        if term_lines:
            self.lines.extend(term_lines[:-1])
            last_term = term_lines[-1]
            self.lines.append(f"{last_term}||")
        else:
            self._emit_line("├─ ||", prefix=opening_prefix)
        self.lines.extend(context_lines)
        self._emit_line(">", prefix=opening_prefix)
        self._cur = []
        return node

    def visit_Lamda(self, node: Lamda):
        self._emit(f"λ{node.di.di.name}:{node.di.prop}.")
        self._newline()
        # The body is the binder's one child.
        self.lines.extend(self._render_child(node.term, is_last=True))
        return node

    def visit_Cons(self, node: Cons):
        self.visit(node.term)
        self._emit("*")
        self.visit(node.context)
        return node

    def visit_Admal(self, node: Admal):
        self.visit(node.context)
        self._emit(f".{node.id.prop}:{node.id.id.name}λ")
        return node

    def visit_Sonc(self, node: Sonc):
        self.visit(node.context)
        self._emit("*")
        self.visit(node.term)
        return node

    def visit_Goal(self, node: Goal):
        self._emit(f"?{node.number}:{labelled_prop(node.prop, node, False)}")
        return node

    def visit_Laog(self, node: Laog):
        self._emit(f"{node.number}:{labelled_prop(node.prop, node, True)}?")
        return node

    def visit_Deleg(self, node: Deleg):
        self._emit(f"!{node.number}:{labelled_prop(node.prop, node, False)}")
        return node

    def visit_Geled(self, node: Geled):
        self._emit(f"{node.number}:{labelled_prop(node.prop, node, True)}!")
        return node

    def visit_ID(self, node: ID):
        self._emit(self._leaf(node))
        return node

    def visit_DI(self, node: DI):
        self._emit(self._leaf(node))
        return node

    @staticmethod
    def _leaf(node) -> str:
        # A citation of a sub-debate (core/dc/share.py) carries what its
        # site captures and cuts, as ``pres.gen`` prints it.
        name = f'{node.name}{getattr(node, "bracket", "")}'
        prop = labelled_prop(node.prop, node, isinstance(node, ID))
        return f"{name}:{prop}" if prop else name

    # -- first-order nodes -------------------------------------------------

    def visit_LamdaFO(self, node: LamdaFO):
        self._emit(f"λ{node.var}:{node.sort}.")
        self._newline()
        # The body is the binder's one child.
        self.lines.extend(self._render_child(node.term, is_last=True))
        return node

    def visit_ConsFO(self, node: ConsFO):
        self._emit(f"{node.fo_term}*")
        self.visit(node.context)
        return node

    def visit_TermsPairFO(self, node: TermsPairFO):
        self._emit(f"({node.witness},")
        self.visit(node.term)
        self._emit(")")
        return node

    def visit_DestructTermsPairFO(self, node: DestructTermsPairFO):
        self._emit(f"({node.var}:{node.sort}).")
        self.visit(node.context)
        return node

    def visit_unhandled(self, node):
        # A node that brings its own printed form - a stub standing for a
        # resolved sub-debate on the shared route (core/dc/instances.py)
        # carries its display name - is printed as that.
        shown = getattr(node, "pres", None)
        self._emit(shown if isinstance(shown, str) and shown else f"Unhandled term type: {type(node)}")
        return node


# Dialectical rendering: the vanilla rendering with every support and attack
# scaffold abbreviated to its debate.  A statement t with supporters a_1..a_n
# reads  SUP(t) ( ++a_1 ... ++a_n )  and with attackers  ATT(t) ( --b_1 ... ),
# PUS and TTA on the context side; the scaffold's wiring is not shown.
dialectical_rendering = Rendering_Semantics('   ', "", "", "", "", "", "", "", "", "", "")

_SCAFFOLD_HEADS = {
    ("supporter", "term"): "SUP",
    ("attacker", "term"): "ATT",
    ("supporter", "context"): "PUS",
    ("attacker", "context"): "TTA",
}


def _match_any_scaffold(node):
    """The paper shapes first, then the legacy M3 shapes the `support` and
    `attack` verbs still build (core/dc/debate_graph.py).  Only meaningful
    top-down, which is how the visitor meets them: it never descends into
    a scaffold's wiring, where a legacy shape could match spuriously."""
    from core.dc.debate_graph import _match_legacy_scaffold, _match_paper_scaffold
    return _match_paper_scaffold(node) or _match_legacy_scaffold(node)


class _DialecticalVisitor(_VanillaVisitor):
    def visit_Mu(self, node: Mu):
        match = _match_any_scaffold(node)
        if match is None:
            return super().visit_Mu(node)
        self._scaffold(node, match)
        return node

    def visit_Mutilde(self, node: Mutilde):
        match = _match_any_scaffold(node)
        if match is None:
            return super().visit_Mutilde(node)
        self._scaffold(node, match)
        return node

    @staticmethod
    def _scion(match):
        """A matched scaffold's scion; a legacy attack scion has its open
        site rerouted to the scaffold's binder, which is not shown, so the
        site is put back (named after that binder)."""
        role, _, prop, _, scion, alt, kind = match
        if match.legacy and role == "attacker":
            from core.dc.debate_graph import _restore_scion
            return _restore_scion(scion, kind, alt, prop, alt)
        return scion

    def _stack(self, scion, side):
        """A supporter scion's alternatives, bottom first: in the stacked
        shape the scion is itself a support scaffold SUP(SUP(P1, P2), P3)
        (core.dc.debate_graph.stack_elements)."""
        match = _match_any_scaffold(scion)
        if match is not None and match[0] == "supporter" and match[1] == side:
            return self._stack(match[3], side) + self._stack(self._scion(match), side)
        return [scion]

    def _scaffold(self, node, match):
        """SUP(base) ( ++a_1 ... ++a_n ): the scaffolds of one role stacked
        on one statement (the legacy shape nests them through the original,
        the stacked shape through the supporter scion) read as one.  In a
        labelled term the head carries the statement's label as its
        proposition would: SUP{L}, {L}PUS."""
        role, side, prop = match[0], match[1], match[2]
        items = []
        while True:
            scion = self._scion(match)
            items[:0] = self._stack(scion, side) if role == "supporter" else [scion]
            base = match[3]
            match = _match_any_scaffold(base)
            if match is None or match[:3] != (role, side, prop):
                break
        head = labelled_prop(_SCAFFOLD_HEADS[(role, side)], node, side == "context")
        mark = "++ " if role == "supporter" else "-- "
        opening_prefix = self.prefix
        base_lines = self._render_lines(base)
        if len(base_lines) == 1:
            self._emit(f"{head}({base_lines[0]}) (")
            self._newline()
        else:
            self._emit(f"{head}(")
            self._newline()
            self.lines.extend(opening_prefix + "   " + line for line in base_lines)
            self._emit_line(") (", prefix=opening_prefix)
        for i, item in enumerate(items):
            is_last = i == len(items) - 1
            self.lines.extend(self._with_prefix(
                self._render_lines(item),
                self._child_prefix(opening_prefix, is_last) + mark,
                self._continuation_prefix(opening_prefix, is_last) + "   ",
            ))
        self._emit_line(")", prefix=opening_prefix)
        self._cur = []


def _instantiated_variable(cell: ConsFO) -> str | None:
    """The variable a universal instantiation ``t * c`` replaces: the first
    name of the quantifier it consumes (its enriched proposition), or None
    where the proposition is unknown or not a universal."""
    from core.ac.prop import PQuant, Prop, PropError
    try:
        quant = Prop.parse(str(cell.prop).strip()) if cell.prop else None
    except PropError:
        return None
    if isinstance(quant, PQuant) and quant.quantifier.value == "forall":
        return quant.names[0]
    return None


def _fill(template: str, **values: str) -> str:
    """Substitute @name placeholders, as decorations and connectives do."""
    for name, value in values.items():
        template = template.replace(f"@{name}", value)
    return template


class _NLVisitor(ProofTermVisitor):
    def __init__(
        self,
        semantic: Rendering_Semantics,
        lines: list[str],
        indent: int = 0,
        *,
        declarations: Mapping[str, str] | None = None,
        decorations: Mapping[str, str] | None = None,
        bound_ids: set[str] | None = None,
        bound_dis: set[str] | None = None,
    ):
        super().__init__()
        self.semantic = semantic
        self.lines = lines
        self.indent = indent
        self.declarations = declarations or {}
        self.decorations = decorations or {}
        self.bound_ids = set(bound_ids or ())
        self.bound_dis = set(bound_dis or ())
        # The site that is the bottom alternative of the support (attack)
        # scaffold being rendered: what stands if no supporter does.
        self._fallback = None

    def _indent_str(self) -> str:
        return self.semantic.indentation * self.indent

    def _owed_phrase(self, term, default: str) -> str:
        """The phrase of an owed site: by its label where the style phrases
        sites by label (``owed_phrases``), else the style's one phrase."""
        phrases = getattr(self.semantic, "owed_phrases", None)
        if not phrases:
            return default
        label = getattr(term, "label", None) or (
            term.number if str(getattr(term, "number", "")) in ("IN", "OUT", "UNDEC") else None)
        if label is None and term is self._fallback:
            return phrases.get("fallback", phrases["pending"])
        return phrases.get(label or "pending", phrases["pending"])

    def _mark_fallback(self, binder_name: str, site, site_class) -> None:
        """Mark ``site`` as the bottom alternative of a scaffold: the term (or
        context) of an alternative's binder (``alt1``, ...) that is an owed
        site itself."""
        if (getattr(self.semantic, "owed_phrases", None) and _ALT_BINDER.match(binder_name or "")
                and isinstance(site, site_class)):
            self._fallback = site

    def _cite_line(self, term) -> str | None:
        """A citation of a named sub-debate (``render ... shared``,
        core/dc/share.py): the style's pointer to it, else None."""
        if getattr(term, "debate", None) is None or not getattr(term, "cites", None):
            return None
        template = getattr(self.semantic, "cite_template", "see @name: @prop")
        return (template.replace("@name", str(term.cites))
                .replace("@prop", self._render_prop(term.prop)))

    def _render_fo(self, term) -> str:
        return render_fo_term(term, self.decorations)

    def _binder_text(self, name: str, *, gap: bool) -> str:
        """What follows the proposition of a mu/mu~ line: the style's binder
        template, else the name in brackets."""
        template = getattr(self.semantic, "binder_template", None)
        if template:
            return template.replace("@binder", name)
        return f"{' ' if gap else ''}({name})"

    def _bound_leaf(self, prefixes, name: str) -> str:
        """A reference to a binder in scope: the style's phrase, with the name
        in place of @binder, or followed by it."""
        phrase = self._leaf_prefix(prefixes, 1)
        return phrase.replace("@binder", name) if "@binder" in phrase else phrase + name

    def _render_prop(self, prop: str | None) -> str:
        return render_prop(
            prop,
            self.declarations,
            self.decorations,
            getattr(self.semantic, "connective_templates", {}),
        )

    @staticmethod
    def _leaf_prefix(prefix, index: int) -> str:
        if isinstance(prefix, (list, tuple)):
            return prefix[index]
        return prefix

    def _with_indent(self, delta: int, node):
        old = self.indent
        self.indent = old + delta
        try:
            return self.visit(node)
        finally:
            self.indent = old

    def visit(self, node):
        if self._try_pattern_render(node):
            return node
        return super().visit(node)

    def _try_pattern_render(self, node) -> bool:
        if node is None:
            return False
        registry = getattr(self.semantic, "pattern_registry", None)
        if registry is None or not registry.renderers:
            return False

        context = PatternRenderContext(
            indent=self.indent,
            indentation=self.semantic.indentation,
            declarations=self.declarations,
            decorations=self.decorations,
            render_prop=self._render_prop,
            render_node=self._render_node_lines,
        )
        result = registry.try_render(node, context)
        if result is None:
            return False

        self.lines.extend(result.lines)
        return True

    def _render_node_lines(self, node, indent_delta: int = 0) -> list[str]:
        child_lines: list[str] = []
        child_visitor = _NLVisitor(
            self.semantic,
            child_lines,
            indent=self.indent + indent_delta,
            declarations=self.declarations,
            decorations=self.decorations,
            bound_ids=self.bound_ids,
            bound_dis=self.bound_dis,
        )
        child_visitor.visit(node)
        return child_lines

    def visit_Mu(self, term: Mu):
        indent_str = self._indent_str()
        # if term.id.name == "stash":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[1] + f"{term.prop}" + " in")
        #     self._with_indent(1, term.term)
        #     return term
        # if term.id.name == "support":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}")
        #     self._with_indent(1, term.term)
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[2])
        #     self._with_indent(1, term.context)
        #     return term
        # if re.compile(r"alt\d").match(term.id.name):
        #     self.visit(term.term)
        #     self.visit(term.context)
        #     return term
        # if term.id.name == "undercut":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
        #     self._with_indent(1, term.context)
        #     self.lines.append(f"{indent_str}" + self.semantic.Mu[3])
        #     self._with_indent(1, term.term)
        #     return term

        self.lines.append(f"{indent_str}" + self.semantic.Mu[0] + f"{self._render_prop(term.prop)}"
                          + self._binder_text(term.id.name, gap=bool(getattr(self.semantic, "owed_phrases", None))))
        self._mark_fallback(term.id.name, term.term, Goal)
        old_bound_ids = self.bound_ids
        self.bound_ids = set(old_bound_ids)
        self.bound_ids.add(term.id.name)
        try:
            self._with_indent(1, term.term)
            self._with_indent(1, term.context)
        finally:
            self.bound_ids = old_bound_ids
        return term

    def visit_Mutilde(self, term: Mutilde):
        indent_str = self._indent_str()
        # if term.di.name == "issue":
        #     self.lines.append(f"{indent_str}" + self.semantic.Mutilde[0] + f"{term.prop}")
        #     self.visit(term.term)
        #     self.lines.append(f"{indent_str}" + self.semantic.Mutilde[1])
        #     self.visit(term.context)
        #     return term
        # if term.di.name == "adapter":
        #     self.visit(term.term)
        #     self.lines.append(f"{indent_str}".removesuffix(self.semantic.indentation) + self.semantic.Mutilde[2])
        #     return term
        # if re.compile(r"alt\d").match(term.di.name):
        #     self.visit(term.term)
        #     self.visit(term.context)
        #     return term

        self.lines.append(f"{indent_str}" + self.semantic.Mutilde[0] + f"{self._render_prop(term.prop)}"
                          + self._binder_text(term.di.name, gap=True))
        self._mark_fallback(term.di.name, term.context, Laog)
        old_bound_dis = self.bound_dis
        self.bound_dis = set(old_bound_dis)
        self.bound_dis.add(term.di.name)
        try:
            self._with_indent(1, term.term)
            self._with_indent(1, term.context)
        finally:
            self.bound_dis = old_bound_dis
        return term

    def visit_Lamda(self, term: Lamda):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Lamda + f"{self._render_prop(term.di.prop)}" + f"({term.di.di.name})")
        old_bound_dis = self.bound_dis
        self.bound_dis = set(old_bound_dis)
        self.bound_dis.add(term.di.di.name)
        try:
            self.visit(term.term)
        finally:
            self.bound_dis = old_bound_dis
        return term

    def visit_Admal(self, term: Admal):
        old_bound_ids = self.bound_ids
        self.bound_ids = set(old_bound_ids)
        self.bound_ids.add(term.id.id.name)
        try:
            self.visit(term.context)
        finally:
            self.bound_ids = old_bound_ids
        return term

    def visit_Cons(self, term: Cons):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Cons)
        self.visit(term.term)
        self.visit(term.context)
        return term

    # -- first-order nodes -------------------------------------------------
    #
    # A first-order variable is not a proof variable, so it does not join
    # bound_dis/bound_ids -- those decide whether a leaf reads as a reference
    # to an assumption, which an individual never is.

    def visit_LamdaFO(self, term: LamdaFO):
        self.lines.append(
            self._indent_str()
            + _fill(self.semantic.LamdaFO, var=term.var, sort=str(term.sort))
        )
        self.visit(term.term)
        return term

    def visit_DestructTermsPairFO(self, term: DestructTermsPairFO):
        self.lines.append(
            self._indent_str()
            + _fill(self.semantic.DestructTermsPairFO, var=term.var, sort=str(term.sort))
        )
        self.visit(term.context)
        return term

    def visit_ConsFO(self, term: ConsFO):
        template, var = self.semantic.ConsFO, _instantiated_variable(term)
        if "@var" in template and var is None:
            template = getattr(self.semantic, "ConsFO_anonymous", None) or template
        self.lines.append(
            self._indent_str() + _fill(template, witness=self._render_fo(term.fo_term), var=var or "")
        )
        self.visit(term.context)
        return term

    def visit_TermsPairFO(self, term: TermsPairFO):
        self.lines.append(
            self._indent_str() + _fill(self.semantic.TermsPairFO, witness=self._render_fo(term.witness))
        )
        self.visit(term.term)
        return term

    def visit_Goal(self, term: Goal):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self._owed_phrase(term, self.semantic.Goal)
                          + f"{self._render_prop(term.prop)}")
        return term

    def visit_Laog(self, term: Laog):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self._owed_phrase(term, self.semantic.Laog)
                          + f"{self._render_prop(term.prop)}")
        return term
    
    def visit_Deleg(self, term: Deleg):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Deleg + f"{self._render_prop(term.prop)}")
        return term
    
    def visit_Geled(self, term: Geled):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Geled + f"{self._render_prop(term.prop)}")
        return term

    def visit_DI(self, term: DI):
        indent_str = self._indent_str()
        cite = self._cite_line(term)
        if cite is not None:
            self.lines.append(f"{indent_str}{cite}")
            return term
        if term.name in self.bound_dis:
            if not getattr(self.semantic, "tree_guides", False):
                indent_str = indent_str.removesuffix(self.semantic.indentation)
            self.lines.append(f"{indent_str}" + self._bound_leaf(self.semantic.DI, term.name))
        else:
            self.lines.append(f"{indent_str}" + self._leaf_prefix(self.semantic.DI, 0) + f"{self._render_prop(term.prop)}" + f" ({term.name})")
        return term

    def visit_ID(self, term: ID):
        indent_str = self._indent_str()
        cite = self._cite_line(term)
        if cite is not None:
            self.lines.append(f"{indent_str}{cite}")
            return term
        falsum = getattr(self.semantic, "falsum_phrase", None)
        if falsum and term.name == "_F_" and term.name not in self.bound_ids:
            self.lines.append(f"{indent_str}{falsum}")
            return term
        if term.name in self.bound_ids:
            if not getattr(self.semantic, "tree_guides", False):
                indent_str = indent_str.removesuffix(self.semantic.indentation)
            self.lines.append(f"{indent_str}" + self._bound_leaf(self.semantic.ID, term.name))
        else:
            self.lines.append(f"{indent_str}" + self._leaf_prefix(self.semantic.ID, 0) + f"{self._render_prop(term.prop)}" + f" ({term.name})")
        return term

    def visit_Sonc(self, term: Sonc):
        indent_str = self._indent_str()
        self.lines.append(f"{indent_str}" + self.semantic.Cons)
        self.visit(term.context)
        self.visit(term.term)
        return term

    def visit_unhandled(self, term):
        indent_str = self._indent_str()
        shown = getattr(term, "pres", None)       # a stub: its display name
        self.lines.append(f"{indent_str}" + (shown if isinstance(shown, str) and shown
                                             else f"Unhandled term type: {type(term)}"))
        return term



def traverse_proof_term(semantic, term, lines, indent, *, declarations=None, decorations=None):
    if semantic is vanilla_rendering:
        _VanillaVisitor(semantic, lines, indent=indent).render(term)
    elif semantic is dialectical_rendering:
        _DialecticalVisitor(semantic, lines, indent=indent).render(term)
    else:
        _NLVisitor(semantic, lines, indent=indent, declarations=declarations, decorations=decorations).visit(term)
