# TODOs and debugging notes

## Architect

### Templates

- Generic TODO
  - ID: T-XXXX
  - Location: file/path[:symbol or function]
  - Summary: short description of the task
  - Why: rationale for the change
  - Suggested approach: concrete steps or outline
  - Dependencies: related tasks or ordering constraints
  - Status: Planned | In progress | Done | Blocked
  - Added: 2025-11-03

- Bug report
  - ID: B-XXXX
  - Location: file/path[:symbol or function]
  - Symptom: what goes wrong (error, incorrect behavior)
  - Why this is a bug: brief reasoning
  - Impact: user-visible effects or risk
  - Reproduction: minimal steps to reproduce (if applicable)
  - Suggested fix: concise change or patch outline
  - Risk: Low | Medium | High (side-effects, compatibility)
  - Status: Open | Fixed | Wontfix
  - Added: 2025-11-03

### Items









## User

### Templates

- Generic TODO
  - ID: U-T-XXXX
  - Location: file/path[:symbol or function]
  - Summary:
  - Why:
  - Suggested approach:
  - Dependencies:
  - Status:
  - Added:

- Bug report
  - ID: U-B-XXXX
  - Location:
  - Symptom:
  - Why this is a bug:
  - Impact:
  - Reproduction:
  - Suggested fix:
  - Risk:
  - Status:
  - Added:

### Items

- Generic TODO
  - ID: U-T-DUMMY-0001
  - Location: <fill by user>
  - Summary: <user to fill>
  - Why: <user to fill>
  - Suggested approach: <user to fill>
  - Dependencies: <user to fill>
  - Status: <user to fill>
  - Added: 2025-11-03
  
- Generic TODO
  - ID: U-T-0001
  - Location: <core/dc/argument.py (for main methods)>
  - Summary: <Implement support and rebut>
  - Why: <These are the two remaining operators of the debate calculus>
  - Suggested approach: <Implement rebut/support grafting and reduction rules>
  - Dependencies: <@Architect: please enter here the dependencies>
  - Status: In progress
  - Added: 2025-11-04
  
- Generic TODO
  - ID: U-T-0002
  - Location: <tests directory>
  - Summary: <Test Cycles and Contrapositive>
  - Why: <We have to check if grafting catches cycles and what happens with contrapositive arguments. >
  - Suggested approach: <Write a test script and run it through the script test>
  - Dependencies: <None>
  - Status: Planned
  - Added: 2025-11-04
  
- Generic TODO
  - ID: U-T-0003
  - Location: <core/dc/argument.py; core/comp/reduce.py>
  - Summary: Implement support() in core/dc/argument.py and its reduction rule in core/comp/reduce.py.
  - Why: Completes debate calculus operations beyond undercut; needed for feature parity.
  - Suggested approach: Add support() method constructing appropriate adapter/grafting; add corresponding reduction rule; mirror undercut guards and logging.
  - Dependencies: Core refactor completed; test scenarios prepared.
  - Status: Obsolete/Merged
  - Added: 2025-11-04
  - Note: Operation implemented via support(); reduction tracked under T-0023.

- Generic TODO
  - ID: U-T-0004
  - Location: <core/dc/argument.py; core/comp/reduce.py>
  - Summary: Implement rebut() in core/dc/argument.py and its reduction rule in core/comp/reduce.py.
  - Why: Enables argumentative rebuttal operation to complete the core trio (undercut/support/rebut).
  - Suggested approach: Add rebut() method and reduction pattern; follow existing affine rule structure and guards to avoid loops; extend tests.
  - Dependencies: Core refactor completed; reducer stable.
  - Status: Obsolete/Merged
  - Added: 2025-11-04
  - Note: Operation implemented via rebut(); reduction tracked under T-0025.

  - Generic TODO
  - ID: U-T-0005
  - Location: <instructions.py>
  - Summary: <Try another ASCII replacement for ⊥>
  - Why: <Would be nice for robustness>
  - Suggested approach: <It might be "false">
  - Dependencies: <None>
  - Status: <Planned>
  - Added: 2025-11-04
  
- Generic TODO
  - ID: U-T-0006
  - Location: <Grafting methods @architect please specify>
  - Summary: <Improve logging>
  - Why: <Currently you get something like "Uniformly grafting <core.ac.ast.Mutilde object at 0x7a8f7d851270> on <core.ac.ast.Mu object at 0x7a8f7d7d8d90>" which is not very informative>
  - Suggested approach: <Log presentations of arguments>
  - Dependencies: <None>
  - Status: Done
  - Added: 2025-11-04

- Generic TODO
  - ID: U-T-0007
  - Location: <Reduction systems @architect please specify>
  - Summary: <Upgrade reudction system to debating system. Change name. Implement rebut/support rules. Figure out a nice representation>
  - Why: <Reduction corresponds to "executing" a debate/debating. For explainability this should have a natural language representation>
  - Suggested approach: <In addition to tables, logging during reduction as a start.>
  - Dependencies: <@architect please determine dependencies>
  - Status: In Progress
  - Added: 2025-11-04






- Generic TODO
  - ID: T-0012
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py:execute
  - Summary: Improve logging of argument execution
  - Why: Better traceability of execution lifecycle and failures.
  - Suggested approach: Log start/end, instruction count, and last_state on error at DEBUG; reserve per-instruction echo for TRACE.
  - Dependencies: None
  - Status: Planned
  - Added: 2025-11-11

- Generic TODO
  - ID: T-0013
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py:execute
  - Summary: Clean up argument execution method
  - Why: Reduce duplication; streamline body→instructions→enrichment→rendering; unify theorem/antitheorem handling.
  - Suggested approach: Factor helpers for (a) start command selection, (b) enrichment + proof-term gen, (c) instruction playback; ensure idempotency.
  - Dependencies: None
  - Status: Planned
  - Added: 2025-11-11

- Generic TODO
  - ID: T-0014
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py:focussed_undercut
  - Summary: Re-scope: adopt θ-based undercut that unifies both orientations
  - Why: Unifies adapter/grafting with support; reduces duplication; enables undercut on arbitrary subterms.
  - Suggested approach:
    - Defer concrete implementation to T-0026 (θ-based undercut).
    - Replace focussed_undercut with a θ-based undermine() helper that:
      • chooses orientation by attacker’s root binder (Mu → term, μ′ → context),
      • θ-expands the target uniformly on the attacked issue,
      • builds a single-step adapter (as in support) and chains attacker → adapter → expanded target,
      • keeps focussed_undercut as a temporary alias for backward compatibility.
    - After T-0026 reducer rules land, add end-to-end tests for both orientations.
  - Dependencies: T-0026
  - Status: In progress (operation implemented via attack(); reducer rules tracked in T-0026/T-0023/T-0027)
  - Added: 2025-11-11
  - Updated: 2025-11-18
  - Note: Added Argument.attack() (θ-based). focussed_undercut now delegates to attack. Remaining work: θ-based undercut/rebut reduction rules and tests.

- Generic TODO
  - ID: T-0015
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py; wrap/cli.py; tests
  - Summary: Rename focussed_undercut to undermine (keep alias)
  - Why: Clearer terminology; maintain backward compatibility in CLI/tests.
  - Suggested approach: Implement undermine() delegating to focussed_undercut; update CLI and scripts to prefer “undermine”; keep old name as alias for a deprecation window.
  - Dependencies: T-0014
  - Status: Obsolete
  - Added: 2025-11-11
  - Note: Replaced by attack()/undercut(); no rename to ‘undermine’ planned.

+
+ - Generic TODO
+   - ID: T-0017
+   - Location: fsp/tags/fellowship-0.1.0/core/comp/reduce.py:visit_Mu, visit_Mutilde
+   - Summary: Improve support shape guard diagnostics and robustness
+   - Why: Rules may silently skip due to minor mismatches; better logs and slight guard relaxation will help.
+   - Suggested approach:
+     - Add DEBUG logs printing all involved binder names and immediate child node kinds/props when shape_ok fails.
+     - Optionally accept missing .prop alignment before enrichment (name-based checks only).
+   - Dependencies: None
+   - Status: Planned
+   - Added: 2025-11-12
+
+
+ - Generic TODO
+   - ID: T-0019
+   - Location: tests/basic_support.fspy (new), tests
+   - Summary: Add regression tests for basic_support (Goal and Laog)
+   - Why: Ensure end-to-end reduction triggers correctly and acceptance coloring is consistent.
+   - Suggested approach:
+     - Construct small supporters (affine and non-affine flavors) and a target with a matching assumption; run normalize() and assert resulting pres contains the expected reduced forms.
+     - Add coloring checks on the normalized form (green vs red/yellow) per the new affineness grammar.
+   - Dependencies: T-0016–T-0018
+   - Status: Planned
+   - Added: 2025-11-12
+
+ - Generic TODO
+   - ID: T-0020
+   - Location: fsp/tags/fellowship-0.1.0/core/comp/reduce.py logging
+   - Summary: Consolidate reducer logging for support rules
+   - Why: Easier debugging with consistent before/after and rule tags.
+   - Suggested approach: Ensure “support-keep” and “support-discard” log binder names (alt/aff) and whether supporter was deemed affine; include a one-line pres for the supporter subtree pre/post.
+   - Dependencies: None
+   - Status: Planned
+   - Added: 2025-11-12
+
+
+
+ - Generic TODO
+   - ID: T-0023
+   - Location: fsp/tags/fellowship-0.1.0/core/comp/reduce.py
+   - Summary: Implement alternative-arguments reduction rule (theta-expanded support).
+   - Why: Debate is correctly constructed by support(), but reduction yields the wrong result without a dedicated rule.
+   - Suggested approach:
+     - Add Mu- and Mutilde-oriented rules for θ-expanded support:
+       - μ alt . < μ aff . < t || ID(alt) > ∥ μ′ aff . < Supporter || ID(alt) > >
+       - μ′ alt . < μ aff . < DI(alt) || C > ∥ μ′ aff . < DI(alt) || Supporter > >
+     - Generalize t/C beyond bare Goal/Laog (already relaxed shape guards).
+     - Decide keep/discard via AcceptanceColoringVisitor on the attacker subtree; mirror existing “support-keep”/“support-discard” logging.
+     - Update _has_next_redex to recognize these redexes to unblock “fully simplified” gating.
+   - Dependencies: ThetaExpander; adapter in support(); tests/alternative_arguments.fspy.
+   - Status: Planned
+   - Added: 2025-11-13
+
+ - Generic TODO
+   - ID: T-0024
+   - Location: core/dc/argument.py (support), core/dc/graft.py
+   - Summary: Drop pureness/uniqueness assumptions in support grafting.
+   - Why: Alternative arguments should support arbitrary subterms; multiple sites may match.
+   - Suggested approach:
+     - Allow multi-site support by operating on uniformly θ-expanded targets; ensure capturing substitution chooses innermost binders (already fixed in graft).
+     - Consider optional user controls for scoping (e.g., by number or path) later; default to uniform behavior.
+   - Dependencies: T-0023
+   - Status: Planned
+   - Added: 2025-11-13
+
+ - Generic TODO
+   - ID: T-0025
+   - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py; fsp/tags/fellowship-0.1.0/core/comp/reduce.py
+   - Summary: Implement rebut (operation + reduction rules).
+   - Why: Completes the trio of debate operators beyond undercut/support.
+   - Suggested approach:
+     - Add Argument.rebut() with adapter(s) and chaining pattern analogous to support, oriented appropriately by binder kind.
+     - Add reduction shapes and guards in reducer; reuse acceptance coloring-based keep/discard decisions.
+   - Dependencies: T-0023, T-0024
+   - Status: Planned
+   - Added: 2025-11-13
+
+ - Bug report
+   - ID: B-0004
+   - Location: core/dc/argument.py:support
+   - Symptom: target_kind chosen via is_anti of the supported argument (brittle).
+   - Fix: Determine orientation by supporter’s root binder (Mu → term, Mutilde → context).
+   - Status: Fixed
+   - Added: 2025-11-13
+
+ - Bug report
+   - ID: B-0005
+   - Location: core/dc/argument.py:support (adapter placeholders)
+   - Symptom: Adapter placeholder of incorrect kind prevents selective substitution in step 1.
+   - Fix: Use Goal("s") vs Laog("s") as supporter placeholder based on supporter binder; “some” is the mirror kind to be captured by the θ-expanded alt binder in step 2.
+   - Status: Fixed
+   - Added: 2025-11-13
+
+ - Bug report
+   - ID: B-0006
+   - Location: core/dc/argument.py:support (assumption matching)
+   - Symptom: Matching assumptions against the pre-expansion target fails if θ-expansion is a no-op.
+   - Fix: Execute the expanded target (expanded_arg.execute()) and match against its assumptions.
+   - Status: Fixed
+   - Added: 2025-11-13
+
+ - Bug report
+   - ID: B-0013
+   - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py:execute
+   - Symptom: Executing an argument with no instructions raises AttributeError: 'NoneType' object has no attribute 'get'.
+   - Why this is a bug: When instructions are empty, last_output remains None; execute() then calls _parse_proof_state(last_output), causing a None dereference.
+   - Impact: Empty arguments (or arguments that only open a theorem/antitheorem) cannot be executed; scripts/interactive sessions can crash.
+   - Reproduction: start/end an argument without any instructions; see traceback at _parse_proof_state from execute().
+   - Suggested fix: Initialize last_output with the machine payload returned by the start command; after the loop, if last_output is still None, fall back to the start payload (or issue a no-op command like 'idtac.' to fetch state). Additionally, guard _parse_proof_state against None.
+   - Risk: Low (localized change; does not affect non-empty cases).
+   - Status: Open
+   - Added: 2025-11-18
+
+ - Optional status updates (mark previously planned items as done where applicable):
+   - Update T-0016 (Recognize support redexes in heuristic) → Status: Done.
+   - Note: Mutilde-side support guard generalization has been applied in reducer (visit_Mutilde and _has_next_redex).

- Generic TODO
  - ID: T-0026
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py; fsp/tags/fellowship-0.1.0/core/comp/reduce.py
  - Summary: Reimplement undercut using theta-expansion (unify with support).
  - Why: Unify adapter/grafting mechanism across debate ops; enable undercut on arbitrary subterms (not only default assumptions) and simplify reduction shapes.
  - Suggested approach:
    - Operation:
      - Determine orientation by attacker’s root binder (Mu → term-side, Mutilde → context-side).
      - Theta-expand the target uniformly on the attacked proposition issue using ThetaExpander:
        - attacker is Mu  → mode='term'    (introduces μ alt.< μ aff.<t||alt> ∥ AltC_t >)
        - attacker is μ′  → mode='context' (introduces μ′ alt.< AltT_t ∥ μ′ aff.<alt||c> >)
      - Build an adapter to turn the attacker into a context to be captured by the θ-introduced alt:
        - attacker Mu: adapter1 = μ′ aff.issue.< Laog("s") ∥ Goal("some") >
        - attacker μ′: adapter1 = μ  aff.issue.< Laog("some") ∥ Laog("s") >
          (mirror the placeholder scheme used in support; “s” is replaced by the attacker; “some” is captured by the μ/μ′ alt binder during final graft)
      - Chain attacker → adapter1 (η at root if applicable), then chain adapted attacker → θ-expanded target. Keep exact-match lookup for issue after executing the expanded target (no-op θ case).
      - Deprecate focussed_undercut once θ-based undercut is stable; keep it as a fallback alias temporarily.
    - Reduction:
      - Add θ-based undercut reduction rules mirroring the support ones (generalized t/C, not only Goal/Laog):
        - μ alt . < μ aff . < t || ID(alt) > ∥ μ′ aff . < Attacker || ID(alt) > >
        - μ′ alt . < μ aff . < DI(alt) || C > ∥ μ′ aff . < DI(alt) || Attacker > >
      - Decide keep/discard by AcceptanceColoringVisitor on the attacker subtree (same as support’s red-test).
      - Update _has_next_redex to recognize these undercut redexes for the “fully simplified” heuristic.
    - Tests:
      - Add alternative_undercut scenarios alongside basic undercut; cover both orientations and defeated vs non-defeated attacker.
  - Dependencies: ThetaExpander; existing support θ/rules; acceptance coloring in core/comp/color.py.
  - Status: Planned
  - Added: 2025-11-13

  - Bug report
    - ID: B-0009
    - Location: Acceptance coloring (core/comp/color.py)
    - Symptom: Coloring can fail or mis-handle proof terms containing Admal/Sonc.
    - Why this is a bug: Grammar/AST now include Admal (λ ID:Prop . context) and Sonc (context*term); coloring visitor doesn’t traverse these nodes.
    - Suggested fix: Add visit_Admal and visit_Sonc that recurse into child(ren) and delegate classification accordingly.
    - Status: Fixed
    - Added: 2025-11-14

  - Generic TODO
    - ID: T-0027
    - Location: fsp/tags/fellowship-0.1.0/core/comp/reduce.py:visit_Mutilde
    - Summary: Add guarded support keep/discard (μ′ orientation) mirroring visit_Mu.
    - Why: Alternative counterarguments need the generalized rule; currently only visit_Mu is guarded.
    - Suggested approach: Apply the same left-is-default/defeated conditions on inner_mu.context vs ctx_mt.context; keep “supporter fully simplified” check via _has_next_redex.
    - Status: Planned
    - Added: 2025-11-14

  - Generic TODO
    - ID: T-0030
    - Location: fsp/tags/fellowship-0.1.0/core/comp/color.py
    - Summary: Ensure AcceptanceColoringVisitor traverses Admal and Sonc.
    - Why: New nodes introduced by grammar; avoid failures or incorrect coloring.
    - Status: Done
    - Added: 2025-11-14

  - Bug report
    - ID: B-0010
    - Location: support/_theta_expand logging
    - Symptom: Confusion from unchanged printed term after θ-expansion despite structural change.
    - Fix: Centralized θ-expansion into Argument._theta_expand to always regenerate pres; warn on no-op.
    - Status: Fixed
    - Added: 2025-11-14

  - Bug report
    - ID: B-0011
    - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py:_theta_expand
    - Symptom: Prover error “A symbol named alt is already defined” after θ-expansion; duplicate binder names (alt/aff) introduced multiple times in the same theorem.
    - Why this is a bug: ThetaExpander inserts μ/μ′ binders with fixed names; repeated occurrences collide during instruction generation.
    - Suggested fix: Run FreshenBinderNames immediately after theta-expansion to uniquify all binder names (alt → alt2, aff → aff2, …) with capture-avoiding renaming.
    - Risk: Low (renaming preserves bound occurrences; reducer guards compare names within the same scope).
    - Status: Fixed
    - Added: 2025-11-18

  - Generic TODO
    - ID: T-0032
    - Location: tests (new)
    - Summary: Add regression tests for alternative counterarguments instruction generation.
    - Why: Validate correct cut propositions (e.g., B-A, A-C) and elim ordering with Sonc/Admal.
    - Suggested approach: Small end-to-end scripts exercising θ-expanded counterarguments; assert generated instruction sequences.
    - Status: Planned
    - Added: 2025-11-14

  - Generic TODO
    - ID: T-0033
    - Location: fsp/tags/fellowship-0.1.0/core/comp/reduce.py:ArgumentTermReducer
    - Summary: Add options for rewriting alternative arguments that fall through guarded rules.
    - Why: Some θ-expanded alternative-argument cases remain undecided by keep/discard guards; we need a deterministic fallback and a global toggle.
    - Suggested approach:
      - Extend ArgumentTermReducer.__init__ with two parameters:
        - evaluation_strategy: Literal["call-by-name", "call-by-value"] = "call-by-name"
          - call-by-name: prioritize the right-hand side (supporter/attacker) in μ/μ′ alternative-argument patterns.
          - call-by-value: prioritize the left-hand side (original some_arg/some_ctx).
        - simplify_alternative_arguments: bool = True
          - If False, skip any fallback simplification of alternative-argument structures; leave them unreduced when guards deem them ambiguous.
      - Store on self.evaluation_strategy and self.simplify_alternative_arguments.
      - In visit_Mu and visit_Mutilde, in the “ambiguous” branches where current guards log “no action”, add:
        - If not self.simplify_alternative_arguments: do nothing (fall through).
        - Else choose a deterministic rewrite according to evaluation_strategy:
          - call-by-name: keep the right-hand (supporter/attacker) subtree; discard the left.
          - call-by-value: keep the left-hand (original) subtree; discard the right.
        - Tag table rows as “alt-cbn” or “alt-cbv” for traceability.
      - Do not change guarded keep/discard behavior; the fallback applies only when the guards neither keep nor discard.
      - Optional: expose these options in wrapper/CLI (flags: --eval-str {cbn,cbv}, --no-simplify-alt) later.
    - Dependencies: T-0023, T-0027
    - Status: Planned
    - Added: 2025-11-14

- Generic TODO
  - ID: T-B-0014
  - Location: core/dc/graft.py; core/dc/argument.py:[support|attack|rebut|chain]
  - Summary: Add a closure method that argumentatively closes an argument/debate D by recusively grafting each subargument of D onto D and its subarguments.
  - Why: In generalized support/attack (alternative arguments, rebut), grafting is only one-step: the scion is grafted at the targeted issue in the opponent, but subarguments that refute dependencies (e.g., defaults) are not recursively propagated. This allows an attacker arg1 that relies on a default D to successfully attack arg2 on issue A even when a subargument of arg1 on A (subarg1) refutes D; the refutation of D is missing from the composed debate. Rationally speaking, debate composition must embed the full evidential structure. If subarg1 refutes D used by arg2 (or vice versa), that refutation must be present after composition; otherwise normalization and acceptance coloring can yield incorrect keep/discard outcomes. While we want to be able to represent such partial debates, we should provide a service to close a debate term and ensure rationality.
  - Impact: Incorrect normalized debates and acceptance results; debates may wrongly keep defeated branches; subsequent reductions may assume closure and misbehave.
  - Reproduction:
    - Build arg1 that concludes A but uses a default D internally.
    - Within arg1’s subargument for A, include subarg1 that refutes D.
    - Build arg2 targeted on A.
    - Perform support/attack/rebut: result lacks the propagation of subarg1’s refutation of D into arg2’s side.
  - Suggested fix (this still needs design):
    - Implement recursive “graft-closure” across both arguments:
      1) After initial graft (scion into target at issue A), collect newly introduced open sites on both sides.
      2) For each open site whose proposition matches a subargument’s conclusion on the opposite side, graft that subargument as well.
      3) Alternate sides and repeat until a fixpoint (no new grafts) or assumptions reached.
    - Alternatively, we might dynamically build and keep a separate graph representation of arguments/debates in which propositions are nodes and supports/attacks edges (akin to proof nets or abstract dialectical frameworks). Then the closure of an argument/debate is always available and the concrete argument trees (possibly many per graph) are easily generated.
    - Requirements:
      - Maintain capture-aware substitution and alpha-renaming at each step (reuse _alpha_rename_if_needed).
      - Respect binder kinds and props (Mu→Goal, Mutilde→Laog).
      - Ensure termination: only graft into open Goals/Laogs; track visited (issue, site-id) pairs to avoid re-grafting loops.
    - Integration:
      - Add graft_closure(root_B, root_A) in core/dc/graft.py.
      - Use graft_closure in Argument.chain()/support()/attack()/rebut() instead of a single graft where generalized ops are used.
      - Add tests covering nested refutations across both orientations.
  - Risk: Medium–High (algorithmic complexity and performance).
  - Dependencies: ThetaExpander; FreshenBinderNames; reduction rules T-0023/T-0026/T-0025.
  - Status: Open
  - Added: 2025-11-20

+ - Generic TODO
+   - ID: T-0037
+   - Location: core/comp/reduce.py (ArgumentTermReducer), tests/
+   - Summary: Finalize call-by-onus parallel mode (tests + correctness audit)
+   - Why: We must trust the new strategy; legacy vs onus must converge before switching defaults.
+   - Suggested approach:
+     - Add focused unit/e2e tests for every onus situation (Right/Left shift, CBN, CBV, Fallback; skeptical/credulous variants).
+     - Run legacy and onus-parallel; assert no WARNING divergences for curated baselines; add targeted cases where legacy currently differs and fix classifier rules.
+     - Log-only for now; legacy remains the actual reducer.
+   - Dependencies: None
+   - Status: Planned
+   - Added: 2025-12-10

```
Formal semantics: call-by-onus (verbatim spec)

#### 1.) For any given redex mu alpha.<t||t'>/mu'x.<t||t'>, reduce first the subterm which has the *onus*.    
#### 2.) Else (i.e. if the onus is on none or two or more subterms) fall back on one of three strategies (default: don't simplify): call-by-name (reduce to t[mu'x.t'/alpha]) call-by-value (reduce to t'[mu alpha.t/x]) or don't simplify.    
####   
#### Next we define *onus*. For this purpose we distinguish the following grammatical subcategories (we denote arbitrary closed terms by t and write t' for a fully simplified term t; free variables are denoted by x and alpha, respectively; as usually, "_" stands for an affine binder variable):     
####   
#### Alternative proofs ap ::= mu _.<t'||alpha> | mu'_.<t'||alpha> where alpha is a free variable in ap and t' is not an exception e. Defaults where t' is a Goal are denoted by d. Defeated alternative proofs where t' is an exception are denoted by dap.  
####     
#### Alternative refutations ar ::= mu'_.<x||t'> | mu_.<x||t'> where x is a free variable in ar and t' is not an exception e. Defaults where t' is a Laog are denoted by d. Defeated alternative refutations where t' is an exception e are denoted by dar  
####     
#### Exceptions e ::= mu_.<t'||t> | mu'_.<t||t'> where t' is not itself an exception or a goal/laog.    
####     
#### All other mu/mu'-terms are denoted by m. Other terms including other mu/mu'-terms are denoted by o. Other non-mu/mu' terms by !m    
####     
#### Next we distinguish the following evaluation situations:    
####     
#### Right-shift ::= <t||mu'_.c> | where c can still step.    
####     
#### Left-shift ::= <mu_.c||t>  where c can still step    
####     
#### Call-by-value ::= <ap||e> | <e||mu'_.<e||t>> | <o||e> | <d||ap> | <d||o> | <d||ar> | <ar||e> | <!m||m> | <sonc||admal> | <dap||ap> | <dar||ar>  
####     
#### Call-by-name ::= <e||ap> | <mu_.<t||e>||e> | <e||o> | <ap||d> | <o||d> | <ar||d> | <e||ar> | <m||!m> | <lamda||cons> | <ap||dap> | <ar||dar>  
####     
#### Fallback ::= <m||m> | <ap||ap> | <ar||ar>    
####     
#### Some of these should never occur by construction and should raise a warning:    
####     
#### - <d||o>, <o||d>: generic other terms should never be contraposed to defaults by our argument construction rules.    
#### - <ap||d>, <d||ar>: our convention is that in support operations, the default should be in the head position and supporters in the tail.    
#### - <ap||ar>, <ar||ap> should never occur    
####     
#### Evaluation proceeds as follows:    
####     
#### Right-shift ---> continue by reducing c    
#### Left-shift ---> continue by reducing c    
#### Call-by-name ---> apply mu/lamda reduction rule    
#### Call-by-value ---> apply mu'/admal reduction rule    
#### Fallback ---> no side, has the onus, apply fallback strategy: call-by-name (reduce to t[mu'x.t'/alpha]) call-by-value (reduce to t'[mu alpha.t/x]) or don't simplify (default: don't simplify)    
####     
#### Finally, there is another parameter:    
####     
#### Skeptical/Credulous: in skeptical mode apply the system as is; in credulous mode, move <ap||e> to call-by-name and <e||ar> to call-by-value.  
####   
```
+
+ - Bug report
+   - ID: B-0016
+   - Location: core/comp/reduce.py:_is_ap_node
+   - Symptom: Mutilde branch misclassifies ap; checks node.term is DI and uses node.context as t′.
+   - Why this is a bug: ap for μ′ has shape ⟨ t′ || α ⟩; α is in context (DI), t′ is the term.
+   - Suggested fix: For Mutilde ap, require isinstance(n.context, DI) and set tprime = n.term.
+   - Risk: Low
+   - Status: Open
+   - Added: 2025-12-10
+
+ - Bug report
+   - ID: B-0017
+   - Location: core/comp/reduce.py:_decide_onus
+   - Symptom: Case ⟨ ap || dap ⟩ is marked CBV and also listed under CBN (duplicate and wrong).
+   - Why this is a bug: Spec says CBV includes ⟨ dap || ap ⟩; CBN includes ⟨ ap || dap ⟩.
+   - Suggested fix: Remove CBV branch for (L_ap and R_dap); keep only CBN; deduplicate the check.
+   - Risk: Low
+   - Status: Open
+   - Added: 2025-12-10
+
+ - Bug report
+   - ID: B-0018
+   - Location: core/comp/reduce.py:_decide_onus (shift detection)
+   - Symptom: Right-/Left-shift only inspect one child of c (term or context), not both.
+   - Why this is a bug: Definition requires “c can still step” (either side).
+   - Suggested fix: For right-shift, check right.term and right.context; for left-shift, check left.term and left.context with _has_next_redex.
+   - Risk: Low
+   - Status: Open
+   - Added: 2025-12-10
+
+ - Bug report
+   - ID: B-0019
+   - Location: core/comp/reduce.py:visit_Mu (mu-beta divergence tagging)
+   - Symptom: _maybe_warn_onus_divergence invoked with mismatched where_kind/tag (uses "Mutilde" for Mu) and duplicates.
+   - Why this is a bug: Noise and misleading WARNINGs; should tag once with ("mu-beta", "Mu").
+   - Suggested fix: Call once with where_kind="Mu" and rule_tag="mu-beta"; remove stray duplicate calls.
+   - Risk: Low
+   - Status: Open
+   - Added: 2025-12-10
+
+ - Generic TODO
+   - ID: T-0038
+   - Location: tests (new .fspy), core/comp/reduce.py
+   - Summary: Add regression tests for Admal rule (⟨ E*v || admal α.β ⟩ → ⟨ μ α.<v||β> || E ⟩)
+   - Why: Recently added rule; ensure both Mu and Mutilde cases fire; cover onus-parallel CBV candidate.
+   - Suggested approach: Minimal arguments producing Sonc*Admal shape; assert normalized pres and absence of divergence WARNINGs.
+   - Dependencies: None
+   - Status: Planned
+   - Added: 2025-12-10
+
+ - Generic TODO
+   - ID: T-0039
+   - Location: README.md; wrap/cli.py (docs/usage)
+   - Summary: Document onus-parallel usage and tuning
+   - Why: Make it easy to enable/inspect divergence.
+   - Suggested approach: Add a “Call-by-onus (parallel)” section with env flags:
+     - FSP_EVAL_DISCIPLINE=onus-parallel, FSP_ONUS_FALLBACK, FSP_ONUS_STANCE
+     - Show CLI examples; mention WARNING-level divergence logs.
+   - Dependencies: None
+   - Status: Planned
+   - Added: 2025-12-10
+
+ - Generic TODO
+   - ID: T-0040
+   - Location: core/comp/reduce.py; core/dc/argument.py
+   - Summary: Phase 3 switch: implement evaluation_discipline="onus" (onus leads)
+   - Why: Retire legacy once aligned; keep opt-back to legacy temporarily.
+   - Suggested approach: Make onus decision select actual rewrite; remove legacy special-casing after a deprecation window; default discipline → "onus".
+   - Dependencies: T-0037 (alignment)
+   - Status: Planned
+   - Added: 2025-12-10
+
## Archived
The following items are completed, fixed, or obsolete/merged.

### Completed/Fixed

- Generic TODO
  - ID: T-0034
  - Location: pyproject.toml; Makefile
  - Summary: Package project and expose “acdc” console script; add dev Makefile.
  - Why: Provide a single entrypoint without activating venv; standardize tooling.
  - Suggested approach: pyproject with [project.scripts] acdc=wrap.cli:main; Makefile targets install/cli/test/lint/format/typecheck.
  - Status: Done
  - Added: 2025-12-09

- Bug report
  - ID: B-0015
  - Location: wrap/cli.py: setup_prover/resolve_fsp_path
  - Symptom: pipx-installed acdc failed to find wrap/fellowship/fsp inside site-packages.
  - Fix: Implement resolve_fsp_path with precedence: --fsp → ACDC_FSP/FSP_PATH → packaged path → repo path → PATH.
  - Status: Fixed
  - Added: 2025-12-09

- Generic TODO
  - ID: T-0035
  - Location: README.md
  - Summary: Document binary location, ACDC_FSP persistence, and pipx usage.
  - Why: Users need a stable way to run acdc outside the repo; clarify env var and PATH fallback.
  - Suggested approach: Add “Binary location” and “Persisting ACDC_FSP” sections; include examples and macOS quarantine tip.
  - Status: Done
  - Added: 2025-12-09

- Generic TODO
  - ID: T-0036
  - Location: wrap/cli.py; README.md
  - Summary: Add --fsp flag to override binary path per invocation and document it.
  - Why: Convenient one-off override without touching environment.
  - Suggested approach: argparse --fsp; pass through to setup_prover; log final path; add example to README.
  - Status: Done
  - Added: 2025-12-09

- Bug report
  - ID: B-0001
  - Location: fsp/tags/fellowship-0.1.0/wrap/prover.py:_kv_list_to_dict
  - Symptom: entries parsed from (decls ...) aren’t mapped correctly; potential empty/incorrect decls map.
  - Why this is a bug: code originally wrote out[el][0] = el[1], indexing into a dict entry incorrectly; proper assignment is out[el[0]] = el[1].
  - Impact: declarations from machine payload may not be stored; later lookups can fail.
  - Reproduction: run any command producing decls in machine payload (e.g., declare A:bool.) and inspect ProverWrapper.declarations.
  - Suggested fix: ensure function assigns out[el[0]] = el[1].
  - Risk: Low
  - Status: Fixed
  - Added: 2025-11-03

- Generic TODO
  - ID: T-0001
  - Location: fsp/tags/fellowship-0.1.0/wrapper.py
  - Summary: Wire wrapper.py to wrap/prover.py and remove duplicate ProverWrapper/exception definitions.
  - Why: avoid duplication, centralize machine I/O, reduce maintenance cost.
  - Suggested approach: import ProverWrapper, ProverError, MachinePayloadError from wrap.prover; remove local classes/regex; keep CLI helpers intact.
  - Dependencies: none
  - Status: Done
  - Added: 2025-11-03

- Generic TODO
  - ID: T-0002
  - Location: fsp/tags/fellowship-0.1.0/core/dc/match_utils.py and fsp/tags/fellowship-0.1.0/parser.py
  - Summary: Move match_trees/get_child_nodes/is_subargument from parser.py into core/dc/match_utils.py and import them.
  - Why: consolidate core utilities under core; slim parser shim.
  - Suggested approach: add new module; delete duplicates from parser.py; import from core.dc.match_utils.
  - Dependencies: none
  - Status: Done
  - Added: 2025-11-03

- Generic TODO
  - ID: T-0003
  - Location: fsp/tags/fellowship-0.1.0/parser.py
  - Summary: Remove duplicate acceptance coloring; use pres/color.py as canonical source.
  - Why: eliminate redundancy and drift.
  - Suggested approach: delete AcceptanceColoringVisitor block in parser.py; rely on pres.color imports.
  - Dependencies: none
  - Status: Done
  - Added: 2025-11-03

- Generic TODO
  - ID: T-0004
  - Location: fsp/tags/fellowship-0.1.0/wrap/prover.py and fsp/tags/fellowship-0.1.0/mod/store.py
  - Summary: Proxy only arguments via mod/store; keep declarations per ProverWrapper session.
  - Why: avoid cross-session contamination; declarations must reflect only the current prover session, while arguments can be shared globally.
  - Suggested approach: keep .declarations instance-scoped; proxy .arguments to mod/store; ensure register/get use these; no semantic changes.
  - Dependencies: none
  - Status: Done
  - Added: 2025-11-03

- Generic TODO
  - ID: T-0005
  - Location: fsp/tags/fellowship-0.1.0/pytest.ini; fsp/tags/fellowship-0.1.0/parser.py; fsp/tags/fellowship-0.1.0/wrapper.py
  - Summary: Path hygiene — add pytest.ini (pythonpath=.) and remove sys.path hacks.
  - Why: cleaner import resolution; avoid hidden side effects.
  - Suggested approach: add pytest.ini; delete path-injection blocks at file tops.
  - Dependencies: none
  - Status: Done
  - Added: 2025-11-03

- Generic TODO
  - ID: T-0006
  - Location: fsp/tags/fellowship-0.1.0/parser.py
  - Summary: Keep parser.py as a thin shim and deprecate once callers switch to core/pres.
  - Why: phased migration; minimize breakage.
  - Suggested approach: after T-0001..T-0005, gradually remove shim; optionally add DeprecationWarning.
  - Dependencies: T-0001..T-0005
  - Status: Done
  - Note: Deleted parser.py and wrapper.py; tests import from wrap.* and core.*.
  - Added: 2025-11-03

- Generic TODO
  - ID: T-0007
  - Location: fsp/tags/fellowship-0.1.0
  - Summary: Improve graft/reduce logging to use presentations (pres.gen) instead of object reprs.
  - Why: Current logs show object addresses; using pres strings improves debugging and readability.
  - Suggested approach: Update graft and reduction code to log pres.gen presentations and include before/after snapshots; add logger at TRACE/DEBUG levels.
  - Dependencies: None
  - Status: Done
  - Note: Graft and reducer now log pres-based before/after snapshots at DEBUG; replaced repr-based logs.
  - Added: 2025-11-04

- Generic TODO
  - ID: T-0008
  - Location: repository-wide (tests/, wrap/, fsp/tags/*)
  - Summary: Post-refactor cleanup (move tests, reorganize wrap, ensure fixed fsp path; optional tag rename)
  - Why: Finalize structure and naming after refactor; improve maintainability and module boundaries.
  - Suggested approach:
    - Completed:
      - Move all root-level test files into tests/.
      - Move OCaml Fellowship prover sources under wrap/fellowship/ and rebuild there; delete generated/compiled artifacts from the tag root.
      - Update wrapper to run wrap/fellowship/fsp explicitly (wrap/cli.setup_prover).
      - Move sexp_parser.py under wrap/ and rewire imports; export from wrap/__init__.py.
    - Remaining (optional/later):
      - Consider tag renaming to acdc-0.1.0 and update docs/import paths accordingly.
      - Add small README/CLI notes about the fixed fsp path.
  - Dependencies: Core refactor done; tests import from wrap.* and core.*.
  - Status: Done
  - Added: 2025-11-04

- Generic TODO
  - ID: T-0009
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py; tests/basic_support.fspy
  - Summary: Finish debugging basic_support
  - Why: Execution still emits invalid commands in some scenarios; ensure two-step adapter and chaining are robust.
  - Suggested approach:
    - Validate that only the supporter substitutes in step 1; confirm no leakage into ?1.
    - Add a regression script for basic support that normalizes and renders without errors.
  - Dependencies: T-0010
  - Status: Done
  - Added: 2025-11-11

- Bug report
  - ID: B-0002
  - Location: fsp/tags/fellowship-0.1.0/core/ac/instructions.py:InstructionsGenerationVisitor
  - Symptom: Proof term like
      μ'thesis:A.<thesis:A||μ'aff:A.<μthesis2:A.<μth:A.<r2:C->A||1.2.1:C*th:A>||thesis2:A>||1.2:A>>
    still yields extraneous commands targeting auto binders:
      Instructions deque(['cut (A) thesis', 'cut (A) aff', 'cut (A) thesis2', 'cut (C->A) th', 'axiom r2', 'elim', 'next', 'moxia th', 'moxia thesis2', 'next'])
  - Why this is a bug: The outer theorem/antitheorem binder is injected by the prover; emitting cut/axiom/moxia to thesis[*] is invalid.
  - Impact: Prover errors during adapter/chain execution paths (e.g., support).
  - Suggested fix: See T-0010.
  - Risk: Low
  - Status: Fixed.
  - Added: 2025-11-11

- Generic TODO
  - ID: T-0010
  - Location: fsp/tags/fellowship-0.1.0/core/ac/instructions.py; core/dc/argument.py
  - Summary: Debug instruction generation for thesis/thesisN leakage
  - Why: Avoid invalid commands referencing auto-generated outer binders.
  - Suggested approach:
    - Update InstructionsGenerationVisitor to suppress cut/axiom/moxia when target name matches r'^thesis\\d*$' (case-insensitive).
    - Keep defensive filtering in Argument.execute/chain as a fallback.
    - Add tests for both μ and μ′ wrappers.
  - Dependencies: None
  - Status: Done.
  - Added: 2025-11-11

- Generic TODO
  - ID: T-0011
  - Location: repository-wide logging
  - Summary: Clean up logging statements after debugging
  - Why: Current logs are verbose; normalize levels and messages.
  - Suggested approach: Audit logs in core/dc/argument.py, core/dc/graft.py, core/comp/reduce.py, wrap/cli.py; demote or remove noisy entries.
  - Dependencies: T-0009, T-0010
  - Status: Done
  - Added: 2025-11-11

- Generic TODO
  - ID: T-0016
  - Location: fsp/tags/fellowship-0.1.0/core/comp/reduce.py:_has_next_redex
  - Summary: Recognize support redexes in heuristic
  - Why: Current “fully simplified” checks for supporter subtree never pass because support-specific redexes aren’t recognized.
  - Suggested approach:
    - In Mu branch: detect shape μ alt.< μ aff.<Goal||ID(alt)> ∥ μ′ aff.<Supporter||ID(alt)> > with affineness checks and return True.
    - In Mutilde branch: detect shape μ′ alt.< μ aff.<DI(alt)||Laog> ∥ μ′ aff.<DI(alt)||Supporter> > similarly.
  - Dependencies: None
  - Status: Done
  - Added: 2025-11-12

- Generic TODO
  - ID: T-0018
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py:basic_support
  - Summary: Optionally apply one more η-reduction after step2 or before final graft
  - Why: Some scenarios still carry an outer μ′ thesis wrapper after step2; a root-only η can remove it harmlessly.
  - Suggested approach: After adapted2 is constructed (before final chain), run EtaReducer(verbose=False).reduce(adapted2.body) behind a flag or unconditionally if safe in tests.
  - Dependencies: None
  - Status: Will Not Be Done
  - Added: 2025-11-12

- Generic TODO
  - ID: T-0021
  - Location: fsp/tags/fellowship-0.1.0/core/comp/reduce.py; core/comp/color.py; pres/color.py
  - Summary: Replace “affine?” checks in reducer with “is red?” via AcceptanceColoringVisitor; move coloring into comp.
  - Why: Affineness may not bubble up past non-atomic axioms; coloring provides the correct attack/defeat signal.
  - Suggested approach: Import AcceptanceColoringVisitor in reducer; add _is_red(); swap guards; move color.py to core/comp and make pres/color.py a shim.
  - Dependencies: None
  - Status: Done
  - Added: 2025-11-12

- Generic TODO
  - ID: T-0022
  - Location: core/comp/reduce.py; core/comp/color.py; pres/color.py; pres.tree.py
  - Summary: Dynamically output reduction tree indicating which subarguments were thrown away.
  - Why: Fixing the reduction system means that defence rules now fire and throw away part of the debate term, as intended. However, this breaks the nice tree representations. We need a way to output trees dynamically or come up with a different way of representing debates. Perhaps we can persist them using a new class Debate.
  - Suggested approach: Think about it first. Perhaps debate objects are the way to go.
  - Dependencies: None
  - Status: Done
  - Added: 2025-11-12

- Bug report
  - ID: B-0003
  - Location: fsp/tags/fellowship-0.1.0/core/dc/graft.py:_make_maps
  - Symptom: Uniform graft chose outer binder to capture open Laog/Goal instead of innermost.
  - Fix: Build prop→name maps in push order so deeper binders override earlier ones.
  - Status: Fixed
  - Added: 2025-11-12

- Bug report
  - ID: B-0009
  - Location: Acceptance coloring (core/comp/color.py)
  - Symptom: Coloring can fail or mis-handle proof terms containing Admal/Sonc.
  - Why this is a bug: Grammar/AST now include Admal (λ ID:Prop . context) and Sonc (context*term); coloring visitor doesn’t traverse these nodes.
  - Suggested fix: Add visit_Admal and visit_Sonc that recurse into child(ren) and delegate classification accordingly.
  - Status: Fixed
  - Added: 2025-11-14

- Bug report
  - ID: B-0010
  - Location: support/_theta_expand logging
  - Symptom: Confusion from unchanged printed term after θ-expansion despite structural change.
  - Fix: Centralized θ-expansion into Argument._theta_expand to always regenerate pres; warn on no-op.
  - Status: Fixed
  - Added: 2025-11-14

- Bug report
  - ID: B-0011
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py:_theta_expand
  - Symptom: Prover error “A symbol named alt is already defined” after θ-expansion; duplicate binder names (alt/aff) introduced multiple times in the same theorem.
  - Why this is a bug: ThetaExpander inserts μ/μ′ binders with fixed names; repeated occurrences collide during instruction generation.
  - Suggested fix: Run FreshenBinderNames immediately after theta-expansion to uniquify all binder names (alt → alt2, aff → aff2, …) with capture-avoiding renaming.
  - Risk: Low (renaming preserves bound occurrences; reducer guards compare names within the same scope).
  - Status: Fixed
  - Added: 2025-11-18

- Generic TODO
  - ID: T-0030
  - Location: fsp/tags/fellowship-0.1.0/core/comp/color.py
  - Summary: Ensure AcceptanceColoringVisitor traverses Admal and Sonc.
  - Why: New nodes introduced by grammar; avoid failures or incorrect coloring.
  - Status: Done
  - Added: 2025-11-14

- Generic TODO
  - ID: U-T-0006
  - Location: <Grafting methods @architect please specify>
  - Summary: <Improve logging>
  - Why: <Currently you get something like "Uniformly grafting <core.ac.ast.Mutilde object at 0x7a8f7d851270> on <core.ac.ast.Mu object at 0x7a8f7d7d8d90>" which is not very informative>
  - Suggested approach: <Log presentations of arguments>
  - Dependencies: <None>
  - Status: Done
  - Added: 2025-11-04

- Bug report
  - ID: B-0012
  - Location: Input normalization for falsum (start command, instruction emission)
  - Symptom: Using ASCII "false" in scripts failed with a prover parse error.
  - Resolution: Not a code bug; the test was faulty. No change required.
  - Status: Fixed
  - Added: 2025-11-18

### Obsolete/Merged

- Generic TODO
  - ID: T-0015
  - Location: fsp/tags/fellowship-0.1.0/core/dc/argument.py; wrap/cli.py; tests
  - Summary: Rename focussed_undercut to undermine (keep alias)
  - Why: Clearer terminology; maintain backward compatibility in CLI/tests.
  - Suggested approach: Implement undermine() delegating to focussed_undercut; update CLI and scripts to prefer “undermine”; keep old name as alias for a deprecation window.
  - Dependencies: T-0014
  - Status: Obsolete
  - Added: 2025-11-11
  - Note: Replaced by attack()/undercut(); no rename to ‘undermine’ planned.

- Generic TODO
  - ID: U-T-0003
  - Location: <core/dc/argument.py; core/comp/reduce.py>
  - Summary: Implement support() in core/dc/argument.py and its reduction rule in core/comp/reduce.py.
  - Why: Completes debate calculus operations beyond undercut; needed for feature parity.
  - Suggested approach: Add support() method constructing appropriate adapter/grafting; add corresponding reduction rule; mirror undercut guards and logging.
  - Dependencies: Core refactor completed; test scenarios prepared.
  - Status: Obsolete/Merged
  - Added: 2025-11-04
  - Note: Operation implemented via support(); reduction tracked under T-0023.

- Generic TODO
  - ID: U-T-0004
  - Location: <core/dc/argument.py; core/comp/reduce.py>
  - Summary: Implement rebut() in core/dc/argument.py and its reduction rule in core/comp/reduce.py.
  - Why: Enables argumentative rebuttal operation to complete the core trio (undercut/support/rebut).
  - Suggested approach: Add rebut() method and reduction pattern; follow existing affine rule structure and guards to avoid loops; extend tests.
  - Dependencies: Core refactor completed; reducer stable.
  - Status: Obsolete/Merged
  - Added: 2025-11-04
  - Note: Operation implemented via rebut(); reduction tracked under T-0025.

+ Update summary (2025-11-14, later)
+ - Theta-expansion integration
+   - Added Argument._theta_expand: enrich → θ-expand → regenerate pres; logs no-op warnings. support() now uses it and executes the expanded target for assumption matching.
+ - Reduction (support)
+   - visit_Mu support keep/discard now guarded:
+     - Keep only if supporter undefeated AND (left is default Goal OR left is defeated).
+     - Discard only if supporter defeated AND (left is default Goal OR left undefeated).
+   - Support guards made robust (no AttributeError on unexpected shapes).
+   - Mutilde-side guard generalization for support postponed to next session.
+ - Enrichment/Instructions
+   - visit_Mutilde sets node.contr from context.prop (fixes cut (B-A) th / cut (A-C) bla).
+   - InstructionsGenerationVisitor now handles Admal (λ ID:Prop . context → “elim name.”) and Sonc (context*term → “elim.”).
+ - AST/Grammar/Visitors
+   - Grammar extended with admal and sonc; AST adds Pyh, Admal (Context), and Sonc (Term).
+   - Traversal/renaming/matching updated: visitor, alpha renamers, and match_utils handle Admal/Sonc.
+   - AcceptanceColoringVisitor does not yet traverse Admal/Sonc (planned).
+ - Execution robustness
+   - Argument.execute now tolerates ProverError on the final “next” (ignored; finalize using last successful state).

+ Next session focus (2025-11-15)
+ - Switch undercut to θ-based design (T-0026). Re-scope T-0014 to track CLI aliasing/rename (introduce undermine() using θ-expansion, keep focussed_undercut as an alias) and tests after T-0026 lands.
+ - Implement the μ′-oriented guarded support rule in reduce.visit_Mutilde (mirror of visit_Mu).
+ - Extend AcceptanceColoringVisitor to traverse Admal and Sonc.
+ - Begin rebut using θ-expansion (operation + reduction rules) after support stabilizes.
+ - Add tests for alternative counterarguments’ instruction generation (validate cut propositions).
+
+ Update summary (2025-11-18)
+ - Debate ops:
+   - Added Argument.attack(): θ-based generalization of undercut/rebut.
+   - Orientation by attacker binder (Mu→term, Mutilde→context); adapter is identical in shape to support’s, with placeholders swapped per orientation.
+   - focussed_undercut now delegates to attack (backward compatible).
+ - Theta-expansion: apply FreshenBinderNames after expansion to avoid duplicate binder names (fixes “alt already defined” prover error).
+ - CLI:
+   - Added script-mode commands: attack, rebut (and undercut now calls Argument.undercut()).
+   - Interactive REPL: added support, attack, undercut, rebut commands with optional “on PROP”.
+ - API:
+   - Removed focussed_undercut; undercut() is now a checked specialization of attack().
+ - Open bug:
+   - B-0013: empty arguments (no instructions) crash execute(); guard/fallback needed.
+ - Remaining work:
+   - Implement θ-based undercut/rebut reduction rules (see T-0026, T-0023, T-0027) so normalized results reflect keep/discard decisions.
+
+ Next session focus
+ - Fix B-0013: handle empty-argument execution (use start payload or no-op fetch; guard _parse_proof_state).
+ - Implement reducer rules for θ-based undercut/rebut (T-0026, T-0023, T-0025, T-0027) and mirror in _has_next_redex.
+ - Add end-to-end tests exercising attack()/undercut()/rebut() with θ-expanded targets (Goal/Laog and general subterms).
