# COMP (Core transformations)

Files
- visitor.py
  - ProofTermVisitor: base traversal with visit_Kind hooks.
- enrich.py
  - PropEnrichmentVisitor: attach .prop to Goal/Laog via assumptions; ID/DI via declarations; set flags for negation/falsum.
- reduce.py
  - ArgumentTermReducer: λ-rule; affine μ/μ′ (defence/defeat); guarded μ-β/μ̃-β (skip bare Goal/Laog); snapshot table logging.
  - Helpers: _var_occurs, _is_affine, _subst (closed-term, shadowing-aware), _has_next_redex, _snapshot.
- alpha.py
  - _collect_binder_names, _fresh; _AlphaRename (capture-avoiding rename); FreshenBinderNames (global uniquifier).
- neg_rewrite.py
  - NegIntroRewriter: rewrite outer negation-introduction scaffold to the embedded command.

Notes
- If logging needs pres.gen, import it lazily inside helper methods to avoid pres↔core cycles.
