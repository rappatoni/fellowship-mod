+ Project: Fellowship + Python wrapper refactor and feature work
+ Scope of these notes
  - Summarize all important design decisions, current state, and next steps needed to continue after clearing chat history.
  - Keep under 20k tokens; focuses on what’s needed to resume work quickly.

+ High-level goals
  - Treat counterarguments/antitheorems as first-class μ′ (Mutilde) both in the OCaml prover and Python.
  - Normalize arguments, classify acceptance (green/red/yellow), and render colored proof terms and acceptance trees (proof/NL labels).
  - Robust undercut operation (explicit result naming), followed by support and rebut operations.
  - Refactor Python into modular packages: core (ac/dc/comp), pres, wrap, mod; keep tests working.

+ Update summary (2025-11-20)
+ - Debate model and assumptions (current phase)
+   - Model:
+     - A: the space of arguments (λ̄μ̃μ terms with open goals/laogs).
+     - I = P ∪ d: a set of issues consisting of propositions P (typing term expressions) and antipropositions d (typing context terms).
+     - O: a set of debate operators, i.e., functions f: A × A × I → A.
+     - Debate operation: application of a debate operator to two arguments on an issue, written f(a1, a2, i).
+     - Debate: a sequence o1, …, on such that on+1 = f(on, a, i) for some debate operator f ∈ O.
+   - Assumptions used for current reduction rules:
+     - Purity: For every oi, oj in a debate D, oi = f(a, b, i) iff oj = f(c, d, j); i.e., only one debate operator is used throughout the debate (there exists a fixed f used at every step).
+     - Uniqueness: For all oi, oj in a debate D with oi = f(a, b, i) and oj = g(c, d, j), we have i ≠ j; i.e., each issue is operated on at most once.
+   - Note: We will first implement reduction rules under these assumptions and relax them later.

+ Directory layout (top-level repo root)
  - core/
    - ac/: ast.py (ProofTerm; Mu/Mutilde/Lamda/Cons/Goal/Laog/ID/DI/Hyp), grammar.py (Lark, Transformer)
    - comp/: visitor.py (ProofTermVisitor), enrich.py (PropEnrichmentVisitor), reduce.py (ArgumentTermReducer), alpha.py (alpha renaming, FreshenBinderNames), neg_rewrite.py (NegIntroRewriter)
    - dc/: argument.py (Argument: execute/normalize/chain + support/attack/undercut/rebut), graft.py (graft_single, graft_uniform)
  - pres/: gen.py (.pres generator), nl.py (natural language), color.py (AcceptanceColoringVisitor), tree.py (AcceptanceTreeRenderer)
  - wrap/: wrapper + CLI planned split into wrap/prover.py and wrap/cli.py
  - mod/: store.py for persistence of declarations and arguments
  - tests/: .fspy integration tests (e.g., counterarguments_and_undercut.fspy)
  - pytest.ini recommended with: pythonpath = .

+ OCaml prover notes
  - proof_t3rm = context_or_t3rm (Term t3rm | Context context) to support μ′ at root.
  - machine.ml snapshot:
    - prints proof term in ASCII, Term via pretty_t3rm; Context via pretty_context
    - includes goals, messages, decls, and proof-term-hash
  - For standard theorem: μ thesis:A.<mA||thesis>
  - For antitheorem/counterargument: μ′ thesis:A.<thesis||mA>

+ Python AST/Parsing
  - core/ac/ast.py: node classes with explicit types and basic invariants.
  - core/ac/grammar.py: Lark grammar; Transformer produces AST nodes (Mu/Mutilde/etc.).

+ Visitors and core transformations
  - core/comp/visitor.py: ProofTermVisitor base (pre/post traversal).
  - core/comp/enrich.py (currently placeholder): PropEnrichmentVisitor must be moved here verbatim from parser (enrich Goal/Laog props from assumptions; ID/DI props from declarations; set bound_negation flag for neg elim).
  - core/comp/reduce.py (currently placeholder): ArgumentTermReducer needs full implementation:
    - λ-rule
    - affine μ and μ′ rules (defence/defeat cases)
    - μ-β and μ̃-β with guards to skip bare Goal/Laog cases
    - logging: table rows (INFO) + full proof term (DEBUG)
    - re-enter current node only if sub-simplification progressed; else raise RuntimeError to avoid loops
    - heuristic _has_next_redex consistent with guards
  - core/comp/alpha.py (currently placeholder): implement:
    - _collect_binder_names(node): collect all binder names (Mu.id, Mutilde.di, Lamda.hyp.di.name)
    - _fresh(prefix, taken): generate fresh names (we use suffix numbers)
    - _AlphaRename: capture-avoiding α-renamer; rename binders and bound occurrences only within scope
    - FreshenBinderNames: global freshener to uniquify binders to avoid collisions (especially multiple H1)
  - core/comp/neg_rewrite.py (currently placeholder): NegIntroRewriter should rewrite the outer negation-introduction pattern:
    - μ thesis:¬A . ⟨ μ H1:¬A . ⟨ λ H2:A . μ H3:⊥ . ⟨ H2 ∥ ?k ⟩ ∥ H1 ⟩ ∥ thesis ⟩
    into the command ?k/!k; set changed flag, warnings if not found.

+ Grafting and argument ops
  - core/dc/graft.py: grafting logic in place:
    - _GraftVisitor capturing substitution: along path collects binder stacks:
      - μ (ID) binders capture Laog by ID name; μ̃/λ (DI) binders capture Goal by DI name
    - graft_single(body_B, number, body_A): replace a single open goal/laog by body_A (type-check replacement: body_A root must match target kind/prop; perform _alpha_rename_if_needed before grafting)
    - graft_uniform(body_B, body_A): replace all open goals/laogs matching the scion’s conclusion; same alpha-rename before grafting
    - _alpha_rename_if_needed(scion, root): rename scion binders that collide with root binders using _collect_binder_names/_fresh/_AlphaRename
  - Admal/Sonc are supported across visitors: coloring, alpha-renamers, and graft traversal descend through these nodes.
  - core/dc/argument.py: real Argument class with:
    - execute()/normalize()/render()/chain()
    - support() implemented via θ-expansion (two-step adapter) and chaining
    - attack() implemented via θ-expansion; generalizes undercut and rebut
    - undercut()/rebut() delegate to attack() with precondition checks (Goal/Laog vs non-Goal/Laog)
    - θ-expansion freshens binders (FreshenBinderNames) to avoid alt/aff clashes
    - fields: name, body, conclusion, executed flags, normal_body/representation, assumptions/decls
    - operations: execute(), normalize() (run enrichment, reduce, freshening, generate pres + NL), chain(), undercut(), support(), rebut()
    - undercut semantics:
      - takes name=... (required for resulting argument), on=... optional (exact match); else deduce from dual of attacker’s conclusion
      - construct adapter argument (Mu/Mutilde + alt/aff binders) and chain via graft_uniform
      - exact-match finding of attacked assumption; no substring matching; raise error if not found
    - TODOs: implement support() and rebut() stubs.

+ Substitution helper
  - parser.py _subst() is shadowing-aware only; docstring explicitly states it’s intended for closed terms (as produced by the prover). We avoid implementing full capture-avoidance right now.

+ Reduction/guards and termination
  - The reducer must skip:
    - substituting bare Goal into term (μ̃-β)
    - substituting bare Laog into context (μ-β)
  - _has_next_redex should ignore the above blocked cases so the “case 2 defeat” fires instead of looping.
  - After simplifying attacker argument in affine rules, only re-enter if “before” != “after”; else raise to surface bug.

+ Acceptance coloring (normalized terms only)
  - Grammar:
    - Unattacked_Argument ::= proof/refutation | λ . Goal | Goal * Unattacked | Unattacked * Laog | λ x . Unattacked | Unattacked * Unattacked | μ/μ′ with unattacked children
    - Green ::= Unattacked | μ <Goal||Red> | μ′ <Red||Laog> | λ.Green | Green*Green | μ/μ′ <Green||Green>
    - Red   ::= μ <Goal||Green> | μ′ <Green||Laog> | λ.Red | Red*Not_Red | Not_Red*Red | μ/μ′ <Red||Not_Red> / <Not_Red||Red> | Red*Red
    - Yellow::= μ <Goal||Yellow> | μ′ <Yellow||Laog> | λ.Yellow | Yellow*Green | Green*Yellow | μ/μ′ <Yellow||Green> / <Green||Yellow> | Yellow*Yellow
  - Goals/Laogs are colored:
    - If direct child of μ/μ′, inherit parent’s color
    - In λ./Cons unattacked patterns, inherit parent’s color
    - ID/DI leaves color via classify
  - Delimiters: only binder and delimiters colored; children retain nested colors.
  - Errors include node pres strings.

+ Acceptance trees (pres/tree.py)
  - Node = whole outer term A; label: replace the non-goal/laog side of first eligible μ/μ′ with “...”; child is the argument side
  - NL mode supported with elision; styles: argumentation/dialectical/intuitionistic
  - Edges from child→parent; rankdir=BT; arrowhead vee; color orangered

+ CLI/Wrapper
  - Commands (script + interactive):
    - reduce ARG
    - render ARG | render-nf ARG | color ARG | tree ARG [nl style|pt]
    - support NEW supporter target [on PROP]
    - attack  NEW attacker  target [on PROP]
    - rebut   NEW attacker  target [on PROP]
    - undercut NEW attacker target
  - undercut requires explicit new name; exact-match only
  - Prover I/O: machine mode, prints proof-term ASCII; wrapper parses and uses AST visitors for operations
  - Logging: standard levels; can enable DEBUG to see full normalization steps

+ Packaging and entrypoint (current)
+  - pyproject.toml defines console script: acdc = "wrap.cli:main"
+  - Makefile:
+    - make reset-venv; make install → creates .venv and installs package editable (-e .)
+    - .venv/bin/acdc or make cli ARGS="…" to run without activating venv
+    - optional: make binlink → ./acdc
+  - Global install (optional): pipx install . → acdc globally available
+    - If running outside the repo, set ACDC_FSP to the native binary (wrap/fellowship/fsp) or ensure fsp is on PATH
+  - Binary resolution order (wrap/cli.py):
+    - --fsp flag (if provided) → ACDC_FSP or FSP_PATH env → packaged path → repo path → PATH
+    - Logs the selected path; errors if none is executable.

+ Tests
  - counterarguments_and_undercut.fspy (updated for “undercut NAME attacker target”, multiple undercuts)
  - test1.fspy, normalize_render.fspy, tactics.fspy still used
  - script_test.py drives .fspy execution; strict mode raises ProverError on parse errors

+ Current blockers (must fix placeholders)
  - core/comp/enrich.py: must replace placeholder with real PropEnrichmentVisitor (currently parser.py contains a working version; move it verbatim here and import everywhere from core).
  - core/comp/reduce.py: must replace placeholder with real ArgumentTermReducer plus helpers (_var_occurs, _is_affine, _subst, _has_next_redex, _snapshot). Update imports so wrapper and parser use core.comp.reduce.
  - core/comp/alpha.py: implement collected helpers; ensure FreshenBinderNames works to avoid repeated H1 etc.
  - core/comp/neg_rewrite.py: implement NegIntroRewriter logic (parser.py still has a working implementation; move it).
  - core/dc/graft.py: move real graft logic; remove duplicate from parser.py; ensure wrapper and core.dc import graft_uniform/graft_single from core.

+ Next refactor clarifications
  - Move InstructionsGenerationVisitor from parser.py to core/ac/instructions.py; import from core in wrapper.
  - Split wrapper.py later into wrap/prover.py (I/O) and wrap/cli.py (commands); wrapper.py can re-export for backward compatibility.
  - Ensure persistence uses mod/store.py across wrapper/Argument (register/get arguments, declaration map).

+ Implementation checklist (in order)
  1) Move PropEnrichmentVisitor (parser.py) → core/comp/enrich.py; update imports; delete duplicate from parser.py.
  2) Move ArgumentTermReducer (parser.py) → core/comp/reduce.py (with helpers); update imports; delete duplicate.
  3) Move alpha helpers → core/comp/alpha.py; update imports; delete duplicate.
  4) Move NegIntroRewriter → core/comp/neg_rewrite.py; update imports; delete duplicate.
  5) Move graft visitors & functions → core/dc/graft.py; update imports; delete duplicate.
  6) Implement Argument in core/dc/argument.py (move from wrapper.py); ensure undercut() is the renamed method; support()/rebut() as TODO stubs.
  7) Ensure wrapper imports Argument from core and uses mod/store for persistence.
  8) Move InstructionsGenerationVisitor → core/ac/instructions.py; update imports; delete duplicate.
  9) Add pytest.ini in tag dir: pythonpath = . ; remove any sys.path hacks in parser.py/wrapper.py.
  10) Re-run tests; address any missing exports or import paths.

+ Known pitfalls and how to detect
  - Infinite reduction loops: check re-entry guards and _has_next_redex; DEBUG logs show where it sticks.
  - White tails in colored output: ensure parent color around delimiters; child colors preserved; ID/DI leaves colored.
  - Acceptance coloring errors: messages should include node pres; if leaves error, check direct μ/μ′ case ordering.
  - Grafting mismatches: Only exact proposition match; ensure _alpha_rename_if_needed runs before graft; ensure replacement binder kind matches (Mu for Goal, μ′ for Laog).
  - Tests failing on import paths: run pytest from tag dir; ensure pytest.ini sets pythonpath=..

+ - Support reduction shape sensitivity
+   - Guards require strict name alignment: inner μ.id == ctx μ′.di (aff) and ID/DI(alt) match outer binder name.
+   - Alpha-renaming during graft can change binder names; ensure adapters and support rules use the same canonical names (“aff”, “alt”).
+   - Extra auto-injected thesis/thesisN binders can wrap scions; η-reduce at root as needed.
+
+ Final notes
  - OCaml and Python sides must stay consistent in μ and μ′ shapes, especially for initial proof state and elim commands.
  - Substitution is intentionally limited (closed terms); do not feed open terms with free IDs/DIs into reducer/graft.
  - Keep acceptance coloring/trees operating on normalized proof terms; normalize before color/tree.

+ End of notes

Refactor progress snapshot and next steps

Date: 2025-11-04
+ Update summary (2025-11-04)
+ - Refactor complete:
+   - parser.py and wrapper.py removed; imports now use wrap.* and core.*.
+   - CLI split into wrap/cli.py; prover integration lives in wrap/prover.py.
+   - pytest.ini present; conftest clears global store.arguments per test.
+   - Readme files added for core/, core/ac, core/comp, core/dc, pres/, wrap/, mod/.
+   - Tests updated to canonical imports; suite is green.
+ - Next up: post‑refactor cleanup (directory moves/renames) and then new features (support/rebut).
+
+ Update summary (2025-11-10)
+ - Post‑refactor cleanup completed:
+   - Tests moved under tests/ and passing.
+   - sexp_parser.py moved to wrap/; wrap/prover.py imports from .sexp_parser; wrap/__init__.py re‑exports SexpParser.
+   - Fellowship OCaml sources moved under wrap/fellowship/ and rebuilt; generated/build artifacts removed from the tag root (lexer.ml, parser.ml/mli, *.cmi/*.cmo, fsp at old location, ._bcdi/._d, editor backups).
+   - Wrapper resolves the Fellowship binary via resolve_fsp_path in wrap/cli.py:
+     - order: ACDC_FSP/FSP_PATH env → packaged path (wrap/fellowship/fsp) → repo path (wrap/fellowship/fsp) → PATH “fsp”
+     - setup_prover() uses this resolver; logs the chosen path.
+   - Grafting/reduction logging upgraded to use pres.gen presentations with before/after DEBUG snapshots; explicit INFO no‑op logs added for graft_single/graft_uniform.
+   - ProverWrapper: arguments are proxied via mod.store; declarations remain per‑session; machine payload parsing wired to wrap/sexp_parser.
+   - Coq script.v identified as export helper (not used by runtime/machine mode); left out of the OCaml build move.
+ Update summary (2025-11-12)
+ - Acceptance coloring updated:
+   - Added non-affine “unattacked” cases: μ α.<Goal||t(α)> and μ′ x.<t(x)||Laog>.
+   - Guarded Red/Yellow μ<Goal||…> and μ′<…||Laog> cases to require affineness.
+ - Reducer:
+   - Implemented basic support reductions for both orientations:
+     - μ alt.< μ aff.<Goal||alt> ∥ μ′ aff.<Supporter||alt> >
+     - μ′ alt.< μ aff.<DI(alt)||Laog> ∥ μ′ aff.<DI(alt)||Supporter> >
+     - Case A (supporter non-affine): keep supporter via μ̃-β/μ̃ position.
+     - Case B (supporter affine): discard supporter via μ-β/μ position.
+   - Added EtaReducer (one-step root η) and wired it into basic_support after step1.
+ - Basic support still not triggering in some tests:
+   - Likely causes:
+     - Shape mismatch due to remaining wrappers or binder-name divergence after graft.
+     - Our _has_next_redex lacks support-specific detectors, making “fully simplified” checks overly conservative.
+     - Placement of η only after step1 may not always eliminate extra μ′ wrappers encountered after step2 or final graft.
+ - Plan:
+   - Instrument reducer: log binder names/props at support checks.
+   - Add support redex recognition in _has_next_redex for both μ and μ′ cases.
+   - Consider an extra η at adapted2.body or right before final graft if harmless.
+   - Add minimal end-to-end tests for both Goal- and Laog-support flows.
+
+ Plan to fix “thesis” instruction leakage (outline)
+ - Problem: Instruction generation sometimes emits commands targeting auto-injected outer binders (thesis, thesis2, …), e.g., “cut (A) thesis”, which the prover rejects. This shows up especially on antitheorem-shaped wrappers in support.
+ - Approach:
+   1) Source-level suppression:
+      - In InstructionsGenerationVisitor, never emit cut/axiom/moxia if the target name matches r'^thesis\\d*$' (case-insensitive).
+      - Apply the same guard where Mu/Mutilde default cuts are added.
+   2) Defensive filtering:
+      - Keep a minimal filter in Argument.execute() and chain() to strip any residual thesis* commands coming from legacy or corner cases.
+   3) Tests:
+      - Add regression using the failing example proof-term shape; confirm no thesis* commands are present and execution succeeds.
+   4) Cleanup:
+      - Once stable, reduce filters to the visitor (single source of truth) and keep a lightweight safety net in Argument.
+
+ Next session focus
+ - Verify basic_support end-to-end with both orientations:
+   - Confirm support redex detection triggers and keep/discard rewrites fire.
+   - Add optional second η (before final graft) if needed (T-0018).
+ - Add regression tests for support (Goal/Laog; defeated vs non-defeated) (T-0019).
+ - Improve support guard diagnostics (T-0017).
+ - Cleanup/normalize logging levels in reducer and argument execution (T-0012, T-0013).
+
+ Update summary (2025-11-12, later)
+ - Acceptance coloring is now a computational dependency:
+   - Moved AcceptanceColoringVisitor to core/comp/color.py; pres/color.py is a thin re-export shim.
+   - Reducer now uses “is red?” checks (via AcceptanceColoringVisitor) instead of “affine?” guards.
+ - Support reductions:
+   - Added support redex detection to _has_next_redex for both μ and μ′ shapes to unblock “fully simplified” gating.
+   - Basic support keep/discard rules implemented and firing once shapes match.
+ - Grafting:
+   - Fixed capture priority: innermost binder now captures open Goal/Laog first (maps built in push order).
+ - Eta:
+   - Added EtaReducer (root-only η); applied after support step1 to strip extraneous outer μ′ wrapper.
+ - Open items:
+   - Consider an additional root η on adapted2 before final graft (see T-0018).
+   - Add targeted DEBUG diagnostics when support shape guard fails (see T-0017).
+
+ Current state deltas
+ - core/comp: now includes color.py (AcceptanceColoringVisitor).
+ - pres/color.py: re-exports from core/comp/color.py.

Overview
- Goal: modularize into core (ac/dc/comp), pres, wrap, mod; keep parser.py as a thin compatibility shim until all callers are switched; no semantic changes during refactor.
- Current tests: passing after recent moves; continue with small, reversible steps.

Current state
- core/
  - ac/
    - ast.py: ProofTerm, Term/Context, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Hyp.
    - grammar.py: Lark grammar + ProofTermTransformer. Produces AST from Fellowship proof-term strings.
    - instructions.py: InstructionsGenerationVisitor lowers AST to Fellowship commands.
  - comp/
    - visitor.py: ProofTermVisitor (pre/post traversal).
    - enrich.py: PropEnrichmentVisitor (uses assumptions + declarations; sets bound_negation/Falsum flags).
    - reduce.py: ArgumentTermReducer with helpers (_var_occurs, _is_affine, _subst, _has_next_redex, snapshot). Closed-term substitution by design.
    - alpha.py: _collect_binder_names, _fresh, _AlphaRename; FreshenBinderNames implemented but not yet used in graft.
    - neg_rewrite.py: NegIntroRewriter (pattern-based rewrite for negation-intro).
  - dc/
    - graft.py: graft_single, graft_uniform with capturing substitution and binder freshness via _alpha_rename_if_needed.
    - argument.py: Full Argument class (execute, normalize, render, chain, focussed_undercut, support stub). Wrapper imports this.
  - __init__.py and dc/__init__.py: intentionally minimal to avoid import cycles.

- pres/
  - gen.py: ProofTermGenerationVisitor (sets .pres).
  - nl.py: Rendering_Semantics, traverse_proof_term, pretty_natural; three NL styles (argumentation/dialectical/intuitionistic).
  - color.py: AcceptanceColoringVisitor + pretty_colored_proof_term (operates on normalized terms).
  - tree.py: AcceptanceTreeRenderer + render_acceptance_tree_dot (proof/NL label modes).

- wrap/
  - __init__.py present.
  - prover.py: ProverError, MachinePayloadError, ProverWrapper, MACHINE_BLOCK_RE. Not yet wired into wrapper.py (wrapper still defines its own copy).

- wrapper.py (CLI + runner)
  - Still monolithic, contains ProverWrapper + helpers and CLI commands; imports Argument from core.dc.argument (already moved).
  - Contains sys.path injection for tag-local imports.

- parser.py (shim)
  - Imports types/visitors from core.ac.* and core.comp.*; imports pres.* utilities.
  - Re-exports:
    - ArgumentTermReducer from core.comp.reduce
    - graft_single/graft_uniform from core.dc.graft
    - NegIntroRewriter from core.comp.neg_rewrite
    - InstructionsGenerationVisitor from core.ac.instructions
    - PropEnrichmentVisitor from core.comp.enrich
  - Still contains legacy utilities:
    - pretty_natural, Rendering_Semantics, traverse_proof_term
    - match_trees, get_child_nodes, is_subargument, label_assumption
    - A duplicate AcceptanceColoringVisitor block at the end (to be removed later; pres.color is canonical).

- mod/
  - store.py exists with declarations and arguments dicts (not yet adopted by ProverWrapper).

Guardrails
- No semantic changes during refactor. The reducer guards must remain:
  - Skip μ-β when context is a bare Laog.
  - Skip μ̃-β when term is a bare Goal.
  - Skip λ-rule when argument is a bare Goal.
- Grafting:
  - Always call _alpha_rename_if_needed(scion, root) before graft.
  - Replacement root kind and prop must match the Goal/Laog it replaces (Mu for Goal, Mutilde for Laog).
- Keep core/__init__.py and core/dc/__init__.py minimal to avoid cycles.
- Keep parser.py as a facade until all callers are switched.
- After θ-expansion, always run FreshenBinderNames to avoid alt/aff collisions before instruction generation or grafting.

Incremental next steps (safe, reversible)
1) Finish wrapper split (wire to wrap/prover.py)
- Objective: wrapper.py imports ProverWrapper, ProverError, MachinePayloadError from wrap/prover.py and deletes local duplicates. No behavior change.
- Edits:
  - wrapper.py: add “from wrap.prover import ProverWrapper, ProverError, MachinePayloadError”.
  - wrapper.py: remove local classes ProverError, MachinePayloadError, MACHINE_BLOCK_RE, and ProverWrapper.
- Run tests.

2) Move tree-matching utilities to core and re-export (no behavior change)
- Create core/dc/match_utils.py with verbatim copies of:
  - match_trees(nodeA, nodeB, mapping)
  - get_child_nodes(node)
  - is_subargument(A, B)
- Update parser.py:
  - Remove the duplicate definitions and add “from core.dc.match_utils import match_trees, get_child_nodes, is_subargument”.
- Run tests.

3) Clean parser shim (remove presentation/coloring duplicates)
- Remove duplicate AcceptanceColoringVisitor and any duplicate renderers if still present in parser.py.
- Ensure imports from pres:
  - from pres.color import AcceptanceColoringVisitor, pretty_colored_proof_term
  - from pres.tree import AcceptanceTreeRenderer, render_acceptance_tree_dot
- Optional: add a DeprecationWarning in parser.py on import (skip if tests treat warnings as errors).
- Run tests.

4) Adopt mod/store in ProverWrapper (backward compatible)
- In wrap/prover.py:
  - Add: “from mod import store”.
  - Replace ProverWrapper.declarations and .arguments as properties proxying mod.store:
    - @property declarations -> return store.declarations
    - @property arguments -> return store.arguments
  - Update _update_declarations_from_state to write to store.declarations (reads via self.declarations still OK).
  - Keep register_argument/get_argument methods but operate on store.arguments.
- No change to wrapper CLI; API remains identical.
- Run tests.

5) Path hygiene (remove sys.path injections)
- Add pytest.ini at tag root (if missing):
  - [pytest]
    pythonpath = .
- Remove sys.path manipulation blocks from parser.py and wrapper.py that insert the tag directory into sys.path.
- Run tests from tag root with pytest (imports should still work).

6) Optional: convenient core re-exports (avoid cycles)
- In core/__init__.py, add safe re-exports (do not import core.dc.argument here):
  - from .ac.ast import *
  - from .ac.grammar import Grammar, ProofTermTransformer
  - from .ac.instructions import InstructionsGenerationVisitor
  - from .comp.visitor import ProofTermVisitor
  - from .comp.enrich import PropEnrichmentVisitor
  - from .comp.reduce import ArgumentTermReducer
  - from .comp.alpha import _collect_binder_names, _fresh, _AlphaRename
  - from .comp.neg_rewrite import NegIntroRewriter
  - from .dc.graft import graft_single, graft_uniform
- Keep core/dc/__init__.py minimal.
- Run tests.

7) Deprecate parser.py (final step)
- After all internal code and tests no longer import parser, remove parser.py.
- Until then, keep parser.py as a stable shim re-exporting core/pres symbols used by legacy code and tests.

Notes on known pitfalls and how we handled them
- Circular imports
  - We avoid importing core.dc.argument in core/__init__.py and keep __init__ files minimal.
  - parser.py imports only from core/pres and does not feed back into core.
- Closed-term substitution
  - reduce._subst is shadowing-aware but not fully capture-avoiding; intended for closed terms produced by the prover. Do not feed open terms into reducer/graft.
- Logging
  - reducer snapshot uses pres.gen.ProofTermGenerationVisitor; this comp→pres dependency is acceptable for logging. If needed later, move to lazy import inside _snapshot.

How to resume quickly
- Implement step 1 (wire wrapper to wrap/prover.py) and run tests.
- Proceed one step at a time, committing after each passing step with messages like:
  - refactor(wrapper): import ProverWrapper from wrap.prover; drop local copy
  - refactor(parser): move match utilities to core.dc.match_utils; re-export
  - refactor(parser): remove duplicate coloring/tree; import from pres
  - refactor(wrap): proxy persistence to mod.store
  - refactor: remove sys.path hacks; add pytest.ini

Reference files verified in this snapshot
- core/ac: ast.py, grammar.py, instructions.py
- core/comp: visitor.py, enrich.py, reduce.py, alpha.py, neg_rewrite.py
- core/dc: graft.py, argument.py, __init__.py
- pres: gen.py, nl.py, color.py, tree.py
- wrap: prover.py, wrapper.py (still monolithic; next to be split)
- parser.py (shim with re-exports + some legacy utilities)
- mod: store.py
- tests: tests/test_parser_moxia_utils.py, tests/normalize_render.fspy, tests/counterarguments_and_undercut.fspy

+ Post-refactor cleanup plan (Planned)
+ - Move all remaining test modules located at the tag root (e.g., affine_test.py, mu_reduction_test.py, non_affine_test.py, graft_test.py, script_test.py) into tests/.
+ - Move Fellowship-specific wrapper files into wrap/fellowship:
+   - wrap/prover.py → wrap/fellowship/prover.py
+   - Option A: keep wrap/cli.py generic; Option B: move Fellowship-specific CLI pieces to wrap/fellowship/cli.py if needed later.
+ - Rename tag directory:
+   - fsp/tags/fellowship-0.1.0 → fsp/tags/acdc-0.1.0
+   - Update any hard-coded paths (conftest.load_monolith(), docs, CI).
+ - Ensure pytest.ini (pythonpath=.) continues to resolve imports from the new locations.
+ - Run the full test suite; fix any import path regressions.
+
+ Update summary (2025-11-13)
+ - Alternative arguments (support) construction:
+   - Implemented support() using theta-expansion and two-step adapter.
+   - Orientation now determined by the supporter’s root binder (Mu → term, Mutilde → context); no longer uses is_anti.
+   - Theta-expansion no-op case handled by executing the expanded target first to obtain assumptions for matching.
+   - CLI “support” wired to Argument.support.
+   - New test script tests/alternative_arguments.fspy constructs the debate correctly.
+ - Reduction:
+   - Current reducer still yields an incorrect result for the constructed debate.
+   - Plan: add an alternative-arguments reduction rule recognizing the theta-expanded support redex (both Mu and Mutilde orientations).
+ - Roadmap:
+   1) Add the new reduction rule for alternative arguments.
+   2) Drop pureness/uniqueness assumptions (allow multiple/more general support sites).
+   3) Implement rebut (operation + reduction rules) next.
+
+ Next session focus (2025-11-13)
+ - Implement alternative-arguments reduction rule in core/comp/reduce.py:
+   - Recognize θ-expanded shapes (generalized t/C, not only Goal/Laog):
+     - μ alt . < μ aff . < t || ID(alt) > ∥ μ′ aff . < Supporter || ID(alt) > >
+     - μ′ alt . < μ aff . < DI(alt) || C > ∥ μ′ aff . < DI(alt) || Supporter > >
+   - Keep/discard using acceptance coloring of the attacker subtree; ensure _has_next_redex recognizes these redexes.
+ - Remove pureness/uniqueness assumptions in support: allow multiple matching subterms; plan user-facing controls later if needed.
+ - Begin rebut implementation (operation and reduction rules).
