import logging, copy, os
from typing import Optional, Any, Dict
from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.ast import Admal, Cons, Context, DI, Geled, Goal, Hyp, ID, Laog, Lamda, Mu, Mutilde, ProofTerm, Pyh, Sonc, Term
from core.ac.prop_render import prop_to_command
from core.ac.alt_structure import (
    match_alt_structure,
    match_alternative_counterexample_structure,
    match_application_structure,
    match_defeasible_warrant_structure,
    match_dual_application_structure,
    match_dual_defeasible_warrant_structure,
)
from core.ac.instructions import InstructionsGenerationVisitor
from core.comp.enrich import PropEnrichmentVisitor
from core.comp.reduce import ArgumentTermReducer, EtaReducer
from core.comp.oracle_terms import check_conservativity
from pres.gen import ProofTermGenerationVisitor
from pres.nl import (
    pretty_natural,
    natural_language_rendering,
    dialectical_rendering,
    natural_language_argumentative_rendering,
    pruefschema_rendering,
)
from wrap.prover import ProverError, StrictnessRefused, CitationRefused
from core.dc.cite import (
    CitationError, citation_target, is_strict_citation, cite_at_site, mark_citations,
)

logger = logging.getLogger(__name__)


class Argument:
    r"""Implementation of deductive arguments in \bar{\lamda}\mu\tilde{\mu} calculus. Atomic (counter-)arguments are terms of the (co-)intuitionistic (or (co-)minimal) fragment of the calculus enriched with default assumptions. Argumentation terms are constructed from atomic arguments and the undercut, rebut, support and chain operators.

       Argumentation terms are in normal form if they are \bar{\lamda}\mu\tilde{\mu} normal forms. Eta reduction is optional (TODO). 

Currently, a normalization of an argumentation Arg about issue A returns a non-affine term for A iff the top-level argument for A is skeptically accetable under admissible semantics (?) in the abstract argumentation framework induced by Arg.
    
    
    """
    
    #: Deterministic sequence for the theta-expansion temporaries (was
    #: id(body), a memory address that changed between runs).

    def __init__(self, prover, name: str, conclusion: str, instructions: list = None, rendering = "argumentation", enrich : str = "PROPS", is_anti: bool = False):
        # TODO: some of these stil need type hints.
        self.prover = prover
        self.name = name
        self.conclusion = conclusion #Conclusion of the argument.
        self.instructions = instructions  # List of instruction strings
        self.assumptions = {}  # Dict of Assumptions: {goal_number : {prop : some_str, goal_index : some_int, label : some_str}
        self.delegations = {} # Dict of Delegations: {deleg_number : {prop : some_str, deleg_index : some_int, label : some_str}
        # First-order variable context per goal: {goal_number: {var_name: sort}}.
        # Fellowship reports this as `(env ((name "x")(sort "N")) ...)`; it is
        # what distinguishes a first-order variable bound by a λ over a sort
        # from a proof variable bound by a λ over a proposition.
        self.goal_envs = {}
        self.proof_term = None  # To store the proof term if needed.
        self.enriched_proof_term = None # To store an enriched and/or rewritten proof term if needed.
        self.body: ProofTerm = None # parsed proof term created from proof_term
        self.rendering = rendering # Which rendering should be generated?
        self.enrich = enrich # Proof term enriched with additional type information
        self.representation = None # Natural language representation of the argument based on choice of rendering
        self.executed = False  # Flag to check if the argument has been executed
        self.normal_body           = None  # reduced AST (deep‑copy)
        self.normal_form           = None  # Normalized proof term.
        self.normal_representation = None  # NL rendering of normal form.
        # The debate term unfolded for this argument (aida-unfold-entrypoints)
        # and the evaluated normal form, each with the document revision it
        # was built at: adding an argument or a declaration makes them stale.
        self.unfolded_body = None
        self.unfolded_revision = None
        self.labelled_nf = None
        self.labelled_nf_revision = None
        # Explicit flag recording the user's intent to build a counterargument.
        # Why we need this:
        # - In recording mode we only have plain instruction strings; there is no AST
        #   to inspect when deciding between theorem and antitheorem.
        # - The prover currently prints anti‑theorems as μ … (not μ̃) at the top, so
        #   detecting “anti” from the parsed body root is unreliable.
        # - The start decision must be made before any tactic executes; tactics do not
        #   reliably encode counterargument intent.
        # - Machine state is ephemeral: you only get it after sending the start command,
        #   and you also need to replay the choice later (possibly in a new session).
        # - We need to persist and replay the choice across sessions (render/normalize/
        #   chain), and transient machine state is not available beforehand.
        self.is_anti = is_anti
        # Citations made by `cite NAME` (core/dc/cite.py): [(site, name,
        # strict, proposition)], filled at replay.  A defeasible one leaves the site open to
        # Fellowship and becomes a name leaf in the body.
        self.citations = []
        # Fellowship holds it as a theorem (qed'd), so `axiom NAME` cites it
        # as a strict name.  Only ever true of a strict argument.
        self.citable = False

    @staticmethod
    def _rename_outer_binder(node: ProofTerm, new_name: str) -> ProofTerm:
        if not isinstance(node, (Mu, Mutilde)) or not new_name:
            return node
        old_name = getattr(getattr(node, "id", None), "name", None)
        if isinstance(node, Mutilde):
            old_name = getattr(getattr(node, "di", None), "name", None)
        if not old_name or old_name == new_name:
            return node
        from core.comp.alpha import _AlphaRename
        renamer = _AlphaRename({old_name: new_name})
        if isinstance(node, Mu):
            node.id.name = new_name
        else:
            node.di.name = new_name
        if getattr(node, "term", None) is not None:
            node.term = renamer.visit(node.term)
        if getattr(node, "context", None) is not None:
            node.context = renamer.visit(node.context)
        return node

    def _refresh_from_body(self) -> None:
        self.proof_term = None
        self.enriched_proof_term = None
        self.representation = None
        if self.body is None:
            return
        if self.enrich == "PROPS":
            self.enrich_props()
            self.generate_proof_term()

    @staticmethod
    def _fresh_open_number(prefix: str = "open") -> str:
        if not hasattr(Argument, "_open_counter"):
            Argument._open_counter = 0
        Argument._open_counter += 1
        return f"{prefix}{Argument._open_counter}"

    @staticmethod
    def _binder_names(node: ProofTerm | None) -> set[str]:
        names: set[str] = set()

        def walk(n: ProofTerm | None) -> None:
            if n is None:
                return
            if isinstance(n, Mu):
                names.add(n.id.name)
                walk(n.term)
                walk(n.context)
            elif isinstance(n, Mutilde):
                names.add(n.di.name)
                walk(n.term)
                walk(n.context)
            elif isinstance(n, Lamda):
                names.add(n.di.di.name)
                walk(n.term)
            elif isinstance(n, Admal):
                names.add(n.id.id.name)
                walk(n.context)
            elif isinstance(n, Cons):
                walk(n.term)
                walk(n.context)
            elif isinstance(n, Sonc):
                walk(n.context)
                walk(n.term)

        walk(node)
        return names

    @staticmethod
    def _external_binders_to_open_leaves(node: ProofTerm, external_names: set[str]) -> ProofTerm:
        def walk(n: ProofTerm, local_names: set[str]) -> ProofTerm:
            if isinstance(n, DI):
                if n.name in external_names and n.name not in local_names:
                    return Goal(Argument._fresh_open_number("g"), n.prop)
                return n
            if isinstance(n, ID):
                if n.name in external_names and n.name not in local_names:
                    return Laog(Argument._fresh_open_number("l"), n.prop)
                return n
            if isinstance(n, Mu):
                next_local = set(local_names)
                next_local.add(n.id.name)
                n.term = walk(n.term, next_local)
                n.context = walk(n.context, next_local)
                return n
            if isinstance(n, Mutilde):
                next_local = set(local_names)
                next_local.add(n.di.name)
                n.term = walk(n.term, next_local)
                n.context = walk(n.context, next_local)
                return n
            if isinstance(n, Lamda):
                next_local = set(local_names)
                next_local.add(n.di.di.name)
                n.term = walk(n.term, next_local)
                return n
            if isinstance(n, Admal):
                next_local = set(local_names)
                next_local.add(n.id.id.name)
                n.context = walk(n.context, next_local)
                return n
            if isinstance(n, Cons):
                n.term = walk(n.term, local_names)
                n.context = walk(n.context, local_names)
                return n
            if isinstance(n, Sonc):
                n.context = walk(n.context, local_names)
                n.term = walk(n.term, local_names)
                return n
            return n

        return walk(node, set())

    def _ensure_body_available(self) -> None:
        if self.body is None:
            if not self.executed:
                self.execute()
            if self.body is None:
                raise ValueError("argument body missing")

    @staticmethod
    def _peel_outer_eta(node: ProofTerm) -> ProofTerm:
        """Ignore outer eta-expanded wrappers for projection debate operators.

        Term eta wrappers have shape ``mu alpha.<t||alpha>``.  Context eta
        wrappers have shape ``mu' x.<x||E>``.  The helper peels repeatedly so a
        projection can see the debate structure immediately below such wrappers.
        """
        current = node
        while True:
            if (
                isinstance(current, Mu)
                and isinstance(current.id, ID)
                and isinstance(current.context, ID)
                and current.context.name == current.id.name
            ):
                current = current.term
                continue
            if (
                isinstance(current, Mutilde)
                and isinstance(current.di, DI)
                and isinstance(current.term, DI)
                and current.term.name == current.di.name
            ):
                current = current.context
                continue
            return current

    def _project_argument(self, body: ProofTerm, name: Optional[str], *, source_body: ProofTerm | None = None) -> "Argument":
        body = copy.deepcopy(body)
        if source_body is not None:
            external_names = self._binder_names(source_body)
            body = self._external_binders_to_open_leaves(body, external_names)
        conclusion = getattr(body, "prop", None)
        if conclusion is None:
            raise ValueError("projected body has no proposition")

        projected = Argument(
            self.prover,
            name or f"{self.name}_projection",
            conclusion,
            rendering=self.rendering,
            enrich=self.enrich,
            is_anti=isinstance(body, Context),
        )
        projected.body = body
        projected.assumptions = copy.deepcopy(self.assumptions)
        projected.delegations = copy.deepcopy(self.delegations)
        projected.normal_body = None
        projected.normal_form = None
        projected.normal_representation = None
        projected.executed = False
        return projected

    def out(self, index: int, name: Optional[str] = None) -> "Argument":
        if not isinstance(index, int):
            raise SyntaxError("out index must be an integer")
        self._ensure_body_available()
        body = self._peel_outer_eta(self.body)
        alt = match_alt_structure(body)
        if alt is not None:
            if index < 0 or index >= len(alt.elements):
                raise IndexError("index out of range")
            return self._project_argument(alt.elements[index], name, source_body=self.body)
        if isinstance(body, Term):
            if index == 0:
                return self._project_argument(body, name, source_body=self.body)
            raise IndexError("index out of range")
        raise ValueError("top level is not an `AltStructure`")

    def tou(self, index: int, name: Optional[str] = None) -> "Argument":
        if not isinstance(index, int):
            raise SyntaxError("tou index must be an integer")
        self._ensure_body_available()
        body = self._peel_outer_eta(self.body)
        alt = match_alternative_counterexample_structure(body)
        if alt is not None:
            if index < 0 or index >= len(alt.elements):
                raise IndexError("index out of range")
            return self._project_argument(alt.elements[index], name, source_body=self.body)
        if isinstance(body, Context):
            if index == 0:
                return self._project_argument(body, name, source_body=self.body)
            raise IndexError("index out of range")
        raise ValueError("top level is not an `AlternativeCounterexampleStructure`")

    def sub(self, name: Optional[str] = None) -> "Argument":
        self._ensure_body_available()
        body = self._peel_outer_eta(self.body)
        application = match_application_structure(body)
        if application is None:
            raise ValueError("top level is not an application structure")
        return self._project_argument(application.argument, name, source_body=self.body)

    def bus(self, name: Optional[str] = None) -> "Argument":
        self._ensure_body_available()
        body = self._peel_outer_eta(self.body)
        application = match_dual_application_structure(body)
        if application is None:
            raise ValueError("top level is not a dual application structure")
        return self._project_argument(application.condition, name, source_body=self.body)

    def attacker(self, name: Optional[str] = None) -> "Argument":
        self._ensure_body_available()
        body = self._peel_outer_eta(self.body)
        warrant = match_defeasible_warrant_structure(body)
        if warrant is None:
            raise ValueError("top level is not a defeasible warrant structure")
        return self._project_argument(warrant.exception, name, source_body=self.body)

    def regatta(self, name: Optional[str] = None) -> "Argument":
        self._ensure_body_available()
        body = self._peel_outer_eta(self.body)
        warrant = match_dual_defeasible_warrant_structure(body)
        if warrant is None:
            raise ValueError("top level is not a dual defeasible warrant structure")
        return self._project_argument(warrant.support, name, source_body=self.body)

    def _eta_reduce_body(self) -> None:
        if self.body is None:
            return
        self.body = EtaReducer(verbose=False).reduce(self.body)
        self._refresh_from_body()

    def _synthetic_root_name(self) -> str | None:
        if isinstance(self.body, Mu):
            return getattr(getattr(self.body, "id", None), "name", None)
        if isinstance(self.body, Mutilde):
            return getattr(getattr(self.body, "di", None), "name", None)
        return None

    def execute(self, *, declare: bool = False, preserve_input_body: bool = False) -> None:
        """Sends the sequence of instructions corresponding to an argument (possibly generated from its body) to the Fellowship prover and populates the argument's proof term, body, assumptions, and renderings from the prover output. Can be used for type-checking (TODO).

        If *declare* is true, finish the replay with ``qed.`` so Fellowship
        registers the theorem/antitheorem under this argument's name.  The
        default keeps the historical wrapper behaviour and discards the open
        theorem after extracting the proof state.

        If *preserve_input_body* is true, replay is used as a checker: after
        successful replay, keep the parsed input AST instead of replacing it
        with Fellowship's reconstructed proof term.
        """
        logger.info("Executing argument '%s'.", self.name)
        if self.executed:
            logger.warning("Argument '%s' has already been executed.", self.name)
            return
        if self.instructions == None:
            if self.body == None:
                raise Exception("Argument instructions and body missing.")
            else:
                self.enrich_props()
                generator = InstructionsGenerationVisitor(root_name=self._synthetic_root_name())
                self.instructions = generator.return_instructions(self.body)
                logger.debug("Generated instructions for argument '%s': %s", self.name, self.instructions)

        preserved_input_body = copy.deepcopy(self.body) if preserve_input_body and self.body is not None else None

        # Decide whether to start a theorem or an antitheorem.
        if (self.body and isinstance(self.body, Mutilde)) or getattr(self, "is_anti", False):
            prop = self.body.prop if (self.body and isinstance(self.body, Mutilde)) else self.conclusion
            logger.info("Starting antitheorem '%s' for issue '%s'", self.name, prop)
            start_cmd = f'antitheorem {self.name} : ({prop_to_command(prop)}).'
        else:
            logger.info("Starting theorem '%s' for issue '%s'", self.name, self.conclusion)
            start_cmd = f'theorem {self.name} : ({prop_to_command(self.conclusion)}).'
        start_payload = self.prover.send_command(start_cmd)
        output = start_payload
        # Execute each instruction.  Ordinary Fellowship commands are batched
        # by default to avoid one pexpect round-trip per proof step.
        last_output = start_payload
        instr_list = list(self.instructions)
        total = len(instr_list)
        use_batch_replay = (
            os.getenv("FSP_BATCH_REPLAY", "1").lower() not in {"0", "false", "no"}
            and hasattr(self.prover, "send_commands")
        )
        use_quiet_replay = (
            use_batch_replay
            and os.getenv("FSP_QUIET_REPLAY", "1").lower() not in {"0", "false", "no"}
            and hasattr(self.prover, "send_commands_quiet_final")
        )
        pending_commands: list[str] = []

        def flush_pending() -> None:
            nonlocal last_output
            if not pending_commands:
                return
            if use_quiet_replay:
                output = self.prover.send_commands_quiet_final(pending_commands)
            else:
                output = self.prover.send_commands(pending_commands)
            last_output = output
            logger.trace("Batched prover output: %s", output)
            pending_commands.clear()

        stepwise = False        # once something is cited, one instruction at a time
        for i, instr in enumerate(instr_list):
            try:
                cited = citation_target(self.prover, instr)
            except CitationError as e:
                flush_pending()
                self.prover.send_command('discard theorem.')   # leave the prover clean
                raise CitationRefused(str(e)) from e
            if cited is not None:
                flush_pending()
                try:
                    site, side, prop = self._cite_site(last_output, cited, instr)
                except CitationError as e:
                    self.prover.send_command('discard theorem.')
                    raise CitationRefused(str(e)) from e
                if is_strict_citation(self.prover, cited.name):
                    # Fellowship holds it: it closes the goal itself.
                    last_output = self.prover.send_command(
                        f"{'axiom' if side == 'rhs' else 'moxia'} {cited.name}.")
                    self.citations.append((site, cited.name, True, prop))
                    continue
                # Defeasible: the site stays open to Fellowship and becomes a
                # name leaf afterwards.  Move off it, unless it is the only goal.
                self.citations.append((site, cited.name, False, prop))
                stepwise = True
                if self._goal_count(last_output) > 1:
                    last_output = self.prover.send_command('next.')
                continue
            if stepwise:
                # A cited site is closed from the user's point of view: never
                # let a later step land on it.
                last_output = self._step_off_cited_sites(last_output, instr)
            norm = instr.strip().lower()
            command = instr.strip() + '.'
            # Preserve the historical special case: if a final "next"
            # raises, ignore it.  Keep that one command on the old single
            # command path so we still know exactly which command failed.
            should_single_step = (i == total - 1 and norm == "next") or not use_batch_replay or stepwise
            if use_batch_replay and not should_single_step:
                pending_commands.append(command)
                continue
            flush_pending()
            try:
                output = self.prover.send_command(command)
                last_output = output
                logger.trace("Prover output: %s", output)
            except ProverError as e:
                if i == total - 1 and norm == "next":
                    logger.warning("Ignoring ProverError on final 'next': %s", e)
                    break
                raise
        flush_pending()
        # Capture the assumptions (open goals) using the last successful state
        if last_output is None:
            last_output = start_payload
        output = last_output
        self._parse_proof_state(output)
        if preserved_input_body is not None:
            self.body = preserved_input_body
            if isinstance(self.body, (Mu, Mutilde)) and getattr(self.body, "prop", None):
                self.conclusion = self.body.prop
        else:
            # Parse proof term returned by Fellowship.
            grammar = Grammar()
            parsed = grammar.parser.parse(self.proof_term)
            transformer = ProofTermTransformer()
            self.body = transformer.transform(parsed)
            if isinstance(self.body, (Mu, Mutilde)) and getattr(self.body, "prop", None):
                self.conclusion = self.body.prop
                self._rename_outer_binder(self.body, self.name)
        for site, cited_name, strict, prop in self.citations:
            if strict:
                continue
            try:
                self.body = cite_at_site(self.body, site, cited_name, prop)
            except CitationError as e:
                self.prover.send_command('discard theorem.')
                raise CitationRefused(str(e)) from e
            logger.info("Argument '%s' cites '%s' at site %s.", self.name, cited_name, site)
        strict_cited = {name for _, name, strict, _ in self.citations if strict}
        if strict_cited:
            # Fellowship left the name as an axiom leaf; mark it as the
            # citation it was, so it regenerates as `cite` and reads as one.
            mark_citations(self.body, lambda n: n in strict_cited)
        #Generate natural language representation
        render_context = {
            "declarations": getattr(self.prover, "declarations", {}),
            "decorations": getattr(self.prover, "decorations", {}),
        }
        if self.rendering == "argumentation":
            self.representation = pretty_natural(self.body, natural_language_argumentative_rendering, **render_context)
        elif self.rendering == "dialectical":
            self.representation = pretty_natural(self.body, dialectical_rendering, **render_context)
        elif self.rendering == "intuitionistic":
            self.representation = pretty_natural(self.body, natural_language_rendering, **render_context)
        elif self.rendering == "pruefschema":
            self.representation = pretty_natural(self.body, pruefschema_rendering, **render_context)
        open_sites = self.open_sites()
        if declare is True and open_sites:
            self.prover.send_command('discard theorem.')
            raise StrictnessRefused(
                f"'{self.name}' is not strict, so it cannot be a theorem: "
                + "; ".join(open_sites)
                + ". Discharge them, or end with `end argument` to keep it as a defeasible argument."
            )
        if declare is True or (declare == "auto" and not open_sites):
            try:
                self.prover.send_command('qed.')
            except Exception:
                try:
                    self.prover.send_command('discard theorem.')
                except Exception:
                    pass
                raise
            self.citable = True
            logger.info("'%s' is strict; Fellowship now holds it as a theorem.", self.name)
        else:
            self.prover.send_command('discard theorem.')
        logger.info("Argument '%s' executed.", self.name)
        logger.info("")  # spacer before proof term artifact
        logger.info("Argument term: '%s'.", self.proof_term)
        logger.info("")  # spacer after proof term artifact
        if self.enrich == "PROPS":
            self.enrich_props()
            self.generate_proof_term()
        self.executed = True

    @staticmethod
    def _normalize_pt_to_unicode(pt_ascii: str) -> str:
        """Map Fellowship's ASCII fallbacks to the Unicode tokens your
        parser expects.  The examples observed were:
          • 'μ'   encoded as ";:"   (two chars)
          • 'μ̃'  (mu-tilde) encoded as ";:'" or ";:\'"
          • 'λ'   encoded as '\\' (backslash)
        Adjust if your printer uses slightly different sentinels.
        """
        s = pt_ascii
        # unify the two spellings we saw for mu-tilde first
        s = s.replace(";:\'", "μ'" )
        s = s.replace(";:'",  "μ'" )   # plain apostrophe variant
        # plain mu next (do *after* mu-tilde)
        s = s.replace(";:",   "μ")
        # lambda
        s = s.replace("\\",   "λ")
        # `~` and `false` used to be rewritten to `¬` and `⊥` here.  They no
        # longer are: the grammar accepts both spellings, and the substitution
        # was a blind str.replace that corrupted any identifier containing
        # them -- `μfalsehood:A.<a||falsehood>` became `μ⊥hood:A.<a||⊥hood>`.
        return s

    @staticmethod
    def _unquote(atom: Any) -> Any:
        """Strip surrounding double quotes from atoms like '"A"'."""
        if isinstance(atom, str) and len(atom) >= 2 and atom[0] == '"' and atom[-1] == '"':
            return atom[1:-1]
        return atom

    def open_sites(self) -> list:
        """What keeps this argument from being strict, in words: its open
        obligations, its presumptions, and the defeasible arguments it
        cites.  Empty for a strict argument."""
        cited = {site for site, _, strict, _ in self.citations if not strict}
        found = []
        for meta, info in self.assumptions.items():
            if meta in cited:
                continue
            found.append(f"obligation ?{meta}:{info.get('prop')}")
        for meta, info in self.delegations.items():
            found.append(f"presumption !{meta}:{info.get('prop')}")
        for site, name, strict, _ in self.citations:
            if not strict:
                found.append(f"cited defeasible argument '{name}' at {site}")
        return found

    @staticmethod
    def _goal_list(state):
        goals = state.get("goals") if isinstance(state, dict) else None
        if isinstance(goals, list) and goals and goals[0] == "goal":
            goals = [goals]
        return goals or []

    def _goal_count(self, state) -> int:
        return len(self._goal_list(state))

    def _goal_metas(self, state) -> list:
        metas = []
        for goal in self._goal_list(state):
            for item in goal[1:]:
                if isinstance(item, list) and len(item) == 2 and item[0] == "meta":
                    metas.append(self._unquote(item[1]))
        return metas

    def _focused_goal(self, state):
        """(meta, active-prop, side) of the focused goal, or None."""
        goals = self._goal_list(state)
        if not goals:
            return None
        try:
            index = int(state.get("current-goal-index", 1)) - 1
        except (TypeError, ValueError):
            index = 0
        goal = goals[index] if 0 <= index < len(goals) else goals[0]
        fields = {item[0]: self._unquote(item[1]) for item in goal[1:]
                  if isinstance(item, list) and len(item) == 2}
        return fields.get("meta"), fields.get("active-prop"), fields.get("side")

    def _cite_site(self, state, cited, instr: str):
        """(site, side) of the focused goal a citation fills, checked against
        the cited argument: same side, same proposition."""
        from core.dc.debate_graph import canonical_prop
        focused = self._focused_goal(state)
        if focused is None:
            raise CitationError(f"`{instr}`: no open goal to cite '{cited.name}' for.")
        site, prop, side = focused
        cited_anti = isinstance(cited.body, Mutilde) or getattr(cited, "is_anti", False)
        wants_anti = side == "lhs"
        if cited_anti != wants_anti:
            raise CitationError(
                f"`{instr}`: '{cited.name}' is {'a counterargument' if cited_anti else 'an argument'}, "
                f"but the goal {site} needs {'a refutation' if wants_anti else 'a proof'} of {prop}."
            )
        if canonical_prop(prop) != canonical_prop(cited.conclusion):
            raise CitationError(
                f"`{instr}`: '{cited.name}' concludes {cited.conclusion}, but the goal {site} is {prop}."
            )
        return site, side, prop

    def _step_off_cited_sites(self, state, instr: str):
        """Before ``instr``, move the focus off a cited site with `next.`.
        Refuses when only cited sites are left: the step would work on a
        citation, which `cite` closed as far as the author is concerned."""
        cited = {site for site, _, strict, _ in self.citations if not strict}
        for _ in range(self._goal_count(state) + 1):
            focused = self._focused_goal(state)
            if focused is None or focused[0] not in cited:
                return state
            if set(self._goal_metas(state)) <= cited:
                self.prover.send_command('discard theorem.')
                raise CitationRefused(
                    f"`{instr}` would work on the cited goal {focused[0]}, and no other goal is open; "
                    f"a cited goal is closed by `cite`, so the argument is complete here."
                )
            state = self.prover.send_command('next.')
        return state

    def _parse_proof_state(self, proof_state: Dict[str, Any]) -> None:
        """
        Build {goal_number: {"prop": some_str, "label": None}} from the
        machine payload dict produced by the wrapper.
        Mutatis mutandis for delegations.

        - goal_number := value of (meta ...)
        - prop        := value of (active-prop ...) (quotes removed)
        - env         := value of (env ((name ...)(sort ...)) ...), the
                         first-order variable context, into self.goal_envs
        """
        self.proof_term = self._normalize_pt_to_unicode(proof_state.get('proof-term').strip('"'))
        res = {}
        dels = {}
        envs = {}
        goals = proof_state.get('goals')

        # Normalize: 'goals' may be a single `(goal ...)` list or a list of them.
        def _as_goal_list(gval):
            if isinstance(gval, list) and gval and gval[0] == 'goal':
                return [gval]
            if isinstance(gval, list):
                #return [g for g in gval if isinstance(g, dict)]
                return gval
            return []

        goal_entries = _as_goal_list(goals)

        def _as_env(entries) -> Dict[str, str]:
            """Read `(env ((name "x")(sort "N")) ...)` into {name: sort}.

            Fellowship lists the innermost binder first; dict insertion order
            preserves that.
            """
            env: Dict[str, str] = {}
            for entry in entries:
                if not isinstance(entry, list):
                    continue
                fields = {}
                for pair in entry:
                    if isinstance(pair, list) and len(pair) == 2:
                        fields[pair[0]] = self._unquote(pair[1])
                name, sort = fields.get('name'), fields.get('sort')
                if name is not None and sort is not None:
                    env[name] = sort
            return env

        for g in goal_entries:
        #for g in goals:
            attrs = {}
            env = {}
            for item in g[1:]:
                if isinstance(item, list) and len(item) == 2:
                    k, v = item
                    attrs[k] = self._unquote(v)
                # `env` holds a variable number of entries, so it never matches
                # the 2-element shape above and used to be dropped entirely.
                if isinstance(item, list) and item and item[0] == 'env':
                    env = _as_env(item[1:])
            meta = self._unquote(attrs.get('meta')) if 'meta' in attrs else None
            prop = self._unquote(attrs.get('active-prop')) if 'active-prop' in attrs else None
            kind = self._unquote(attrs.get('kind')) if 'kind' in attrs else None
            if meta:
                envs[meta] = env
            if kind == "delegation":
                if meta and prop is not None:
                    dels[meta] = {"prop": prop, "label": None}
            else:
                if meta and prop is not None:
                    res[meta] = {"prop": prop, "label": None}
        self.assumptions = res
        self.delegations = dels
        self.goal_envs = envs

    # def extract_assumptions(self, output):
    #     #print("Match conclusion to assumption.")
    #     # Parse the output to find open goals (assumptions)
    #     preassumptions = []
    #     # temporarily ignore past proof steps. THIS IS DANGEROUS. While idtac should not change anything, it is risky to change the prover state merely to get a copy of the last output again.
    #     temp_output = self.prover.send_command(f'idtac.', 1)
        
    #     # Find the number of goals
    #     num_goals = self.prover.parse_proof_state(temp_output)['goals']
    #     if num_goals > 0 :
    #         # num_goals = int(goals_match.group(1))
    #         # Extract each goal
    #         goal_pattern_proof = re.compile(r'\|-----\s*(\d\.*.*\d*)\s*(\s*[^:]*:[^\s]*\s*)*\*:([^\s]*)')
    #         goal_pattern_refu = re.compile(r'\*:([^\s]*)\s*(\s*[^:]*:[^\s]*\s*)*\|-----\s*(\d\.*.*\d*)')

    #         match = goal_pattern_proof.search(temp_output)
    #         #print("matching")
    #         #print(match)
    #         antimatch = goal_pattern_refu.search(temp_output)
    #         # print("HERE!")
    #         print(" Match, Antimatch", match, antimatch)
    #         preassumptions = [(match.group(3).strip(), 0, match.group(1).strip())] if match else [(antimatch.group(1).strip()+'_bar', 0, antimatch.group(3).strip())]
    #         i=1
    #         print(" Preassumptions", preassumptions)
    #         #Test the while loop, may be buggy.
    #         while i<num_goals:
    #             #print(i)
    #             #print('WARNING')
    #             temp_output = self.prover.send_command('next.')
    #             #print(temp_output)
    #             plus_match = goal_pattern_proof.search(temp_output)
    #             plus_antimatch = goal_pattern_refu.search(temp_output)
    #             print("Test")
    #             test = goal_pattern_refu.search('2 goals yet to prove! \n *:A \n issue:A \n |-----  1.1.2 ')
    #             print(test)
    #             print(plus_match)
    #             print('ANTI')
    #             print(plus_antimatch)
    #             #print('append attempt')
    #             #print(plus_antimatch.group(1).strip()+'_bar')
    #             if plus_match:
    #                 preassumptions.append((plus_match.group(3).strip(), i, plus_match.group(1).strip()))
    #             else:
    #                 preassumptions.append((plus_antimatch.group(1).strip()+'_bar', i, plus_antimatch.group(3).strip()))
    #             #print(assumptions)
    #             i+=1
    #     assumptions = {}
    #     assumptions = {number.strip() : { "prop" : assumption_text.strip(), "index" : goal_index, "label" : None } for (assumption_text, goal_index, number) in preassumptions}
    #     print("Assumptions extracted:")
    #     print(assumptions)
    #     return assumptions

    def enrich_props(self) -> None:
        """
        Use PropEnrichmentVisitor with assumption_mapping = self.assumptions
        and axiom_props = self.axiom_props to fill .prop for each node.
        """
        logger.info("Enriching argument '%s'.", self.name)
        logger.debug("Declarations: %r", self.prover.declarations)
        visitor = PropEnrichmentVisitor(assumptions=self.assumptions,
                                        axiom_props=self.prover.declarations,
                                        delegations=self.delegations)
        self.body = visitor.visit(self.body)
        logger.info("Argument '%s' enriched.", self.name)
        return

    def generate_proof_term(self) -> None:
        """
        Use ProofTermGenerationVisitor to generate the string representation of an enriched proof term.
        """
        logger.info("Starting proof term generation for enriched argument '%s'", self.name)
        visitor = ProofTermGenerationVisitor()
        self.body = visitor.visit(self.body)
        self.enriched_proof_term = self.body.pres
        logger.info("Finished proof term generation for enriched argument '%s'", self.name)
        logger.info("")  # spacer before enriched proof term
        logger.info("Enriched proof term: '%s'.", self.enriched_proof_term)
        logger.info("")  # spacer after enriched proof term
        return
             
    def normalize(self, enrich: bool = True) -> ProofTerm:
        """Compute and cache the normal form *(body, term, rendering) without mutating *self.body*."""
        if not self.executed:
            self.execute()
        #if self.normal_body is not None:
         #   return self.normal_body

        # 1. deep‑copy then reduce
        red_ast = copy.deepcopy(self.body)
        # The legacy reducer (call-by-onus retired 2026-09-16; sigma-first
        # evaluation lives in core/comp/evaluate.py).
        red_ast = ArgumentTermReducer(
            assumptions=self.assumptions,
            axiom_props=self.prover.declarations,
        ).reduce(red_ast)
        # V2 (propositional-fragment-plan.org): reduction of a strict, closed
        # argument must not leave an open obligation behind.
        check_conservativity(self.body, red_ast, operation=f"normalize('{self.name}')")

        # 2. optionally enrich props/types
        if enrich:
            red_ast = PropEnrichmentVisitor(assumptions=self.assumptions,
                                             axiom_props=self.prover.declarations).visit(red_ast)

        # 3. generate textual proof term
        red_ast = ProofTermGenerationVisitor().visit(red_ast)
        self.normal_body = red_ast
        self.normal_form = red_ast.pres

        # 4. natural‑language rendering
        style = {
            "argumentation": natural_language_argumentative_rendering,
            "dialectical":   dialectical_rendering,
            "intuitionistic": natural_language_rendering,
        }[self.rendering]
        self.normal_representation = pretty_natural(
            red_ast,
            style,
            declarations=getattr(self.prover, "declarations", {}),
            decorations=getattr(self.prover, "decorations", {}),
        )
        return self.normal_body

    def render(self, normalized: bool = False) -> Optional[str]:
        """Return NL rendering.  If *normalized* is True, ensure normal form
        is computed first."""
        if normalized:
            #if self.normal_representation is None:
             #   self.normalize()
            return self.normal_representation
        else:
            if self.representation is None:
                self.execute()          # fills original representation
            return self.representation

    def reduce(self) -> None:
        """Normalise and print proof term + NL; keep originals intact."""
        logger.info("Starting argument reduction for '%s'", self.name)
        self.normalize()
        logger.info("Reduction finished for argument '%s'", self.name)
        logger.info("")  # spacer before proof term
        logger.info("Normal‑form proof term: %s", self.normal_form)
        logger.info("")  # spacer after proof term
