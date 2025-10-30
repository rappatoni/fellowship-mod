+ Project: Fellowship + Python wrapper refactor and feature work
+ Scope of these notes
  - Summarize all important design decisions, current state, and next steps needed to continue after clearing chat history.
  - Keep under 20k tokens; focuses on what’s needed to resume work quickly.

+ High-level goals
  - Treat counterarguments/antitheorems as first-class μ′ (Mutilde) both in the OCaml prover and Python.
  - Normalize arguments, classify acceptance (green/red/yellow), and render colored proof terms and acceptance trees (proof/NL labels).
  - Robust undercut operation (explicit result naming), followed by support and rebut operations.
  - Refactor Python into modular packages: core (ac/dc/comp), pres, wrap, mod; keep tests working.

+ Directory layout (under fsp/tags/fellowship-0.1.0)
  - core/
    - ac/: ast.py (ProofTerm; Mu/Mutilde/Lamda/Cons/Goal/Laog/ID/DI/Hyp), grammar.py (Lark, Transformer)
    - comp/: visitor.py (ProofTermVisitor), enrich.py (PropEnrichmentVisitor), reduce.py (ArgumentTermReducer), alpha.py (alpha renaming, FreshenBinderNames), neg_rewrite.py (NegIntroRewriter)
    - dc/: argument.py (Argument; undercut/support/rebut), graft.py (graft_single, graft_uniform)
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
  - core/dc/graft.py (currently placeholder): move verbatim grafting logic:
    - _GraftVisitor capturing substitution: along path collects binder stacks:
      - μ (ID) binders capture Laog by ID name; μ̃/λ (DI) binders capture Goal by DI name
    - graft_single(body_B, number, body_A): replace a single open goal/laog by body_A (type-check replacement: body_A root must match target kind/prop; perform _alpha_rename_if_needed before grafting)
    - graft_uniform(body_B, body_A): replace all open goals/laogs matching the scion’s conclusion; same alpha-rename before grafting
    - _alpha_rename_if_needed(scion, root): rename scion binders that collide with root binders using _collect_binder_names/_fresh/_AlphaRename
  - core/dc/argument.py (currently placeholder): move the real Argument class from wrapper:
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
  - Commands: reduce, render, render-nf, color, tree [nl style|pt], undercut NEW attacker target
  - undercut requires explicit new name; exact-match only
  - Prover I/O: machine mode, prints proof-term ASCII; wrapper parses and uses AST visitors for operations
  - Logging: standard levels; can enable DEBUG to see full normalization steps

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

+ Final notes
  - OCaml and Python sides must stay consistent in μ and μ′ shapes, especially for initial proof state and elim commands.
  - Substitution is intentionally limited (closed terms); do not feed open terms with free IDs/DIs into reducer/graft.
  - Keep acceptance coloring/trees operating on normalized proof terms; normalize before color/tree.

+ End of notes
