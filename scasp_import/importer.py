from __future__ import annotations

import json
import re
import subprocess
import tempfile
from copy import deepcopy
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from core.ac.ast import Cons, Deleg, DI, Geled, Goal, ID, Mutilde, Mu, ProofTerm, Sonc, Term
from pres.gen import ProofTermGenerationVisitor


class ScaspImportError(Exception):
    """Raised when sCASP output cannot be translated into an AC/DC term."""


@dataclass
class ScaspImportResult:
    """A translated sCASP justification tree plus replay setup metadata."""

    body: ProofTerm
    conclusion: str
    bools: set[str] = field(default_factory=set)
    declarations: dict[str, str] = field(default_factory=dict)
    denials: dict[str, str] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)

    def setup_commands(self, *, include_logic: bool = True) -> list[str]:
        """Render deterministic Fellowship setup commands for replaying ``body``."""
        commands: list[str] = []
        if include_logic:
            commands.append("lk.")
        if self.bools:
            commands.append(f"declare {','.join(sorted(self.bools))}:bool.")
        for name in sorted(self.declarations):
            commands.append(f"declare {name}:{_fmt_decl_prop(self.declarations[name])}.")
        for name in sorted(self.denials):
            commands.append(f"deny {name}:{_fmt_decl_prop(self.denials[name])}.")
        return commands

    def proof_term_string(self) -> str:
        """Render the translated AC/DC AST as a proof-term string."""
        rendered = ProofTermGenerationVisitor(verbose=-1).visit(deepcopy(self.body))
        return rendered.pres

    def to_fspy(
        self,
        *,
        name: str = "imported",
        strict: bool = False,
        include_logic: bool = True,
        include_warnings: bool = True,
    ) -> str:
        """Render a replayable .fspy script for this import result."""
        lines: list[str] = []
        if include_warnings:
            lines.extend(f"# import warning: {warning}" for warning in self.warnings)
        lines.extend(self.setup_commands(include_logic=include_logic))
        strict_part = " strict" if strict else ""
        lines.append(f"register {name}{strict_part} : {self.conclusion} := {self.proof_term_string()}")
        return "\n".join(lines) + "\n"

    def write_fspy(
        self,
        path: str | Path,
        *,
        name: str | None = None,
        strict: bool = False,
        include_logic: bool = True,
        include_warnings: bool = True,
    ) -> Path:
        """Write a replayable .fspy script and return the output path."""
        output_path = Path(path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        script_name = name or _sanitize_name(output_path.stem)
        output_path.write_text(
            self.to_fspy(
                name=script_name,
                strict=strict,
                include_logic=include_logic,
                include_warnings=include_warnings,
            )
        )
        return output_path


@dataclass
class _TranslationState:
    bools: set[str] = field(default_factory=set)
    declarations: dict[str, str] = field(default_factory=dict)
    denials: dict[str, str] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)
    placeholder_counter: int = 0
    binder_counter: int = 0

    def next_placeholder(self) -> str:
        self.placeholder_counter += 1
        return str(self.placeholder_counter)

    def fresh_binder(self, base: str) -> str:
        self.binder_counter += 1
        return f"{base}{self.binder_counter}"

    def add_bool(self, prop: str) -> None:
        self.bools.add(prop)

    def declare(self, name: str, prop: str) -> None:
        existing = self.declarations.get(name)
        if existing is not None and existing != prop:
            raise ScaspImportError(
                f"Conflicting declarations for '{name}': '{existing}' vs '{prop}'"
            )
        self.declarations[name] = prop

    def deny(self, name: str, prop: str) -> None:
        existing = self.denials.get(name)
        if existing is not None and existing != prop:
            raise ScaspImportError(
                f"Conflicting denials for '{name}': '{existing}' vs '{prop}'"
            )
        self.denials[name] = prop

    def warn(self, message: str) -> None:
        self.warnings.append(message)


@dataclass(frozen=True)
class _TranslatedTree:
    term: Term
    prop: str
    atom: str


def atom_to_prop(atom: str) -> str:
    """Map an sCASP atom name to a Fellowship proposition name."""
    if not atom:
        raise ScaspImportError("Cannot translate an empty sCASP atom name")
    safe = _sanitize_name(atom)
    return safe[:1].upper() + safe[1:]


def run_scasp_json(
    path: str | Path,
    *,
    scasp_bin: str = "scasp",
    timeout: float | None = 30.0,
) -> dict[str, Any]:
    """Run sCASP and return the generated justification JSON.

    The installed sCASP may return a non-zero status while still writing a valid
    JSON file.  This function accepts that case and only fails if the JSON file
    is missing or unparsable.
    """
    input_path = Path(path)
    if not input_path.is_file():
        raise ScaspImportError(f"sCASP input file not found: {input_path}")

    with tempfile.TemporaryDirectory(prefix="scasp_import_") as tmpdir:
        stem = Path(tmpdir) / "justification"
        json_path = stem.with_suffix(".json")
        cmd = [
            scasp_bin,
            "--tree",
            "--long",
            f"--json={stem}",
            "--quiet",
            str(input_path),
        ]
        try:
            completed = subprocess.run(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False,
                timeout=timeout,
            )
        except subprocess.TimeoutExpired as e:
            if json_path.is_file():
                try:
                    return json.loads(json_path.read_text())
                except json.JSONDecodeError:
                    pass
            raise ScaspImportError(f"sCASP timed out after {timeout} seconds") from e
        if not json_path.is_file():
            detail = completed.stderr.strip() or completed.stdout.strip() or "no output"
            raise ScaspImportError(f"sCASP did not produce JSON: {detail}")
        try:
            return json.loads(json_path.read_text())
        except json.JSONDecodeError as e:
            raise ScaspImportError(f"sCASP produced invalid JSON: {e}") from e


def import_scasp_file(path: str | Path, *, scasp_bin: str = "scasp") -> ScaspImportResult:
    """Run sCASP on ``path`` and translate positive justifications."""
    return translate_json(run_scasp_json(path, scasp_bin=scasp_bin))


def translate_json(data: dict[str, Any], *, answer_index: int | None = None) -> ScaspImportResult:
    """Translate an sCASP JSON payload to an AC/DC proof-term result.

    By default all positive answers are translated and combined as an enum of
    justification trees.  Passing ``answer_index`` preserves the old
    single-answer selection mode for callers that explicitly need it.
    """
    state = _TranslationState()
    answers = data.get("answers")
    if not isinstance(answers, list):
        raise ScaspImportError("sCASP JSON field 'answers' is not a list")

    if answer_index is not None:
        if not answers:
            raise ScaspImportError("Cannot select answer_index from empty sCASP answers")
        if answer_index < 0 or answer_index >= len(answers):
            raise ScaspImportError(f"answer_index {answer_index} is out of range")
        selected_answers = [(answer_index, answers[answer_index])]
    else:
        selected_answers = list(enumerate(answers))

    if not selected_answers:
        prop = _query_prop(data)
        state.add_bool(prop)
        translated = _empty_positive_answer(prop, state)
        state.warn("sCASP JSON contains no answers; importing empty positive answer enum.")
        return ScaspImportResult(
            body=translated.term,
            conclusion=translated.prop,
            bools=state.bools,
            declarations=state.declarations,
            denials=state.denials,
            warnings=state.warnings,
        )

    translated_answers: list[_TranslatedTree] = []
    conclusion: str | None = None
    for answer_number, answer in selected_answers:
        translated = _translate_answer(answer, answer_number, state)
        if conclusion is None:
            conclusion = translated.prop
        elif translated.prop != conclusion:
            raise ScaspImportError(
                f"Answer {answer_number} concludes '{translated.prop}', expected '{conclusion}'"
            )
        translated_answers.append(translated)

    if not translated_answers:
        prop = _query_prop(data)
        state.add_bool(prop)
        translated = _empty_positive_answer(prop, state)
        state.warn("No positive answer trees found; importing empty positive answer enum.")
        translated_answers.append(translated)
        conclusion = prop

    body = _foldr1_answers(translated_answers, conclusion, state)
    return ScaspImportResult(
        body=body,
        conclusion=conclusion,
        bools=state.bools,
        declarations=state.declarations,
        denials=state.denials,
        warnings=state.warnings,
    )


def _query_prop(data: dict[str, Any]) -> str:
    query = data.get("query")
    if not isinstance(query, list) or len(query) != 1:
        raise ScaspImportError("Empty sCASP answer enum requires exactly one positive atom query")
    item = query[0]
    if not isinstance(item, dict) or item.get("type") != "atom":
        raise ScaspImportError("Empty sCASP answer enum requires exactly one positive atom query")
    value = item.get("value")
    if not isinstance(value, str) or not value:
        raise ScaspImportError("Empty sCASP answer enum query atom is missing a value")
    return atom_to_prop(value)


def _empty_positive_answer(prop: str, state: _TranslationState) -> _TranslatedTree:
    alpha = state.fresh_binder("alpha")
    term = Mu(
        ID(alpha, prop),
        prop,
        Goal(state.next_placeholder(), prop),
        ID(alpha, prop),
    )
    return _TranslatedTree(term=term, prop=prop, atom="0")


def _translate_answer(answer: Any, answer_number: int, state: _TranslationState) -> _TranslatedTree:
    if not isinstance(answer, dict):
        raise ScaspImportError(f"sCASP answer {answer_number} is not an object")
    raw_tree = answer.get("tree")
    if not isinstance(raw_tree, list) or not raw_tree:
        raise ScaspImportError(f"sCASP answer {answer_number} contains no justification tree")

    selected: dict[str, Any] | None = None
    ignored_o_nmr_only = False
    for index, item in enumerate(raw_tree):
        if not isinstance(item, dict):
            state.warn(f"Ignoring non-object top-level tree entry {index} of answer {answer_number}.")
            continue
        atom = _atom_name_from_tree(item)
        if atom == "o_nmr_check":
            ignored_o_nmr_only = True
            state.warn("Ignoring sCASP global constraint node 'o_nmr_check'.")
            continue
        if atom is None:
            state.warn(
                f"Ignoring unsupported top-level tree entry {index} of answer {answer_number}: {_describe_tree(item)}."
            )
            continue
        if selected is None:
            selected = item
        else:
            state.warn(f"Ignoring additional top-level justification tree for atom '{atom}'.")

    if selected is None:
        if ignored_o_nmr_only:
            raise ScaspImportError(
                f"sCASP answer {answer_number} contains only o_nmr_check and no positive justification tree"
            )
        raise ScaspImportError(f"No positive atom justification tree found in answer {answer_number}")

    return translate_tree(selected, state)


def _foldr1_answers(answers: list[_TranslatedTree], prop: str, state: _TranslationState) -> Term:
    if not answers:
        raise ScaspImportError("foldr1 over answers requires at least one translated answer")
    acc = answers[-1].term
    for answer in reversed(answers[:-1]):
        acc = _pair_answers(answer.term, acc, prop, state)
    return acc


def _pair_answers(left: Term, right: Term, prop: str, state: _TranslationState) -> Term:
    alt = state.fresh_binder("alt")
    anon_left = "_"
    anon_right = "_"
    return Mu(
        ID(alt, prop),
        prop,
        Mu(
            ID(anon_left, prop),
            prop,
            left,
            ID(alt, prop),
        ),
        Mutilde(
            DI(anon_right, prop),
            prop,
            right,
            ID(alt, prop),
        ),
    )


def translate_tree(tree: dict[str, Any], state: _TranslationState | None = None) -> _TranslatedTree:
    """Translate a positive sCASP justification tree node.

    This lower-level API accepts an optional translation state.  Callers that
    need replay setup metadata should normally use ``translate_json``.
    """
    own_state = state is None
    state = state or _TranslationState()
    atom = _atom_name_from_tree(tree)
    if atom is None:
        raise ScaspImportError(f"Unsupported sCASP tree node: {_describe_tree(tree)}")
    if atom == "o_nmr_check":
        raise ScaspImportError("Cannot translate global constraint node 'o_nmr_check' as a proof tree")

    prop = atom_to_prop(atom)
    state.add_bool(prop)

    children = tree.get("children", [])
    if not isinstance(children, list):
        state.warn(f"Treating non-list children of atom '{atom}' as empty.")
        children = []

    translated_children: list[_TranslatedTree] = []
    for index, child in enumerate(children):
        if not isinstance(child, dict):
            state.warn(f"Ignoring non-object child {index} of atom '{atom}'.")
            continue
        child_atom = _atom_name_from_tree(child)
        if child_atom is None or child_atom == "o_nmr_check":
            state.warn(f"Ignoring unsupported child {index} of atom '{atom}': {_describe_tree(child)}.")
            continue
        translated_children.append(translate_tree(child, state))

    if not translated_children:
        state.declare(_sanitize_name(atom), prop)
        alpha = state.fresh_binder("alpha")
        term = Mu(
            ID(alpha, prop),
            prop,
            DI(_sanitize_name(atom), prop),
            ID(alpha, prop),
        )
    elif len(translated_children) == 1:
        child = translated_children[0]
        f_name = _f_name(child.prop, prop)
        state.declare(f_name, _imp_prop(child.prop, prop))
        alpha = state.fresh_binder("alpha")
        term = Mu(
            ID(alpha, prop),
            prop,
            DI(f_name, _imp_prop(child.prop, prop)),
            Cons(child.term, ID(alpha, prop)),
        )
    else:
        term = _translate_multi_child(atom, prop, translated_children, state)

    if own_state:
        return _TranslatedTree(term=term, prop=prop, atom=atom)
    return _TranslatedTree(term=term, prop=prop, atom=atom)


def _translate_multi_child(
    atom: str,
    prop: str,
    children: list[_TranslatedTree],
    state: _TranslationState,
) -> Term:
    helper = _helper_prop(prop)
    state.add_bool(helper)
    f_name = _f_helper_name(helper, prop)
    state.declare(f_name, _imp_prop(helper, prop))

    list_minus = _foldr1_barred(children, helper, state)
    alt = state.fresh_binder("alt")
    anon_left = "_"
    anon_right = "_"
    alt_mu = Mu(
        ID(alt, helper),
        helper,
        Mu(
            ID(anon_left, helper),
            helper,
            Deleg(state.next_placeholder(), helper),
            ID(alt, helper),
        ),
        Mutilde(
            DI(anon_right, helper),
            helper,
            Deleg(state.next_placeholder(), helper),
            list_minus,
        ),
    )

    alpha = state.fresh_binder("alpha")
    return Mu(
        ID(alpha, prop),
        prop,
        DI(f_name, _imp_prop(helper, prop)),
        Cons(alt_mu, ID(alpha, prop)),
    )


def _foldr1_barred(
    children: list[_TranslatedTree],
    helper: str,
    state: _TranslationState,
):
    if len(children) < 2:
        raise ScaspImportError("foldr1 requires at least two translated children")
    acc = _barred(children[-1], helper, state)
    for child in reversed(children[:-1]):
        acc = _pair_minus(child, acc, helper, state)
    return acc


def _pair_minus(
    left_child: _TranslatedTree,
    right_context,
    helper: str,
    state: _TranslationState,
):
    alt = state.fresh_binder("alt")
    anon_left = "_"
    anon_right = "_"
    return Mutilde(
        DI(alt, helper),
        helper,
        Mu(
            ID(anon_left, helper),
            helper,
            DI(alt, helper),
            _barred(left_child, helper, state),
        ),
        Mutilde(
            DI(anon_right, helper),
            helper,
            DI(alt, helper),
            right_context,
        ),
    )


def _barred(child: _TranslatedTree, helper: str, state: _TranslationState):
    h_name = _h_name(helper, child.prop)
    h_prop = _minus_prop(helper, child.prop)
    state.deny(h_name, h_prop)

    att = state.fresh_binder("att")
    anon_left = "_"
    anon_right = "_"
    x = state.fresh_binder("x")
    att_context = Mutilde(
        DI(att, child.prop),
        child.prop,
        Mu(
            ID(anon_left, child.prop),
            child.prop,
            DI(att, child.prop),
            Geled(state.next_placeholder(), child.prop),
        ),
        Mutilde(
            DI(anon_right, child.prop),
            child.prop,
            child.term,
            Geled(state.next_placeholder(), child.prop),
        ),
    )

    return Mutilde(
        DI(x, helper),
        helper,
        Sonc(att_context, DI(x, helper)),
        ID(h_name, h_prop),
    )


def _atom_name_from_tree(tree: dict[str, Any]) -> str | None:
    node = tree.get("node")
    if not isinstance(node, dict):
        return None
    if node.get("type") != "atom":
        return None
    value = node.get("value")
    if not isinstance(value, str) or not value:
        return None
    return value


def _describe_tree(tree: dict[str, Any]) -> str:
    node = tree.get("node")
    return repr(node)


def _sanitize_name(name: str) -> str:
    sanitized = re.sub(r"[^a-zA-Z0-9_]", "_", name)
    if not sanitized:
        raise ScaspImportError(f"Cannot sanitize empty name from '{name}'")
    if not re.match(r"[a-zA-Z_]", sanitized):
        sanitized = f"n_{sanitized}"
    return sanitized


def _helper_prop(prop: str) -> str:
    return f"H_{_sanitize_name(prop)}"


def _f_name(source_prop: str, target_prop: str) -> str:
    return f"f_{_sanitize_name(source_prop)}_{_sanitize_name(target_prop)}"


def _f_helper_name(helper: str, target_prop: str) -> str:
    return f"f_{_sanitize_name(helper)}_{_sanitize_name(target_prop)}"


def _h_name(helper: str, prop: str) -> str:
    return f"h_{_sanitize_name(helper)}_{_sanitize_name(prop)}"


def _imp_prop(source: str, target: str) -> str:
    return f"{source} -> {target}"


def _minus_prop(left: str, right: str) -> str:
    return f"{left}-{right}"


def _fmt_decl_prop(prop: str) -> str:
    return f"({prop})"