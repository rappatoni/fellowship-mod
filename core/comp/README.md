# COMP (Core transformations, labelling and evaluation)

Files
- visitor.py
  - ProofTermVisitor: base traversal with visit_Kind hooks.
- enrich.py
  - PropEnrichmentVisitor: attach .prop to Goal/Laog via assumptions; ID/DI via declarations; set flags for negation/falsum.
- alpha.py
  - _collect_binder_names, _fresh; _AlphaRename (capture-avoiding rename); FreshenBinderNames (global uniquifier).
- reduce.py
  - EtaReducer: η-reduction of recorded argument bodies.
- adf_label.py
  - Compiles a debate graph into an ADF (one acceptance condition per statement) and labels it with adf-bdd: grounded, complete, preferred, stable.
- oracle.py
  - The naive ADF oracle, a deliberately direct transcription used to cross-check adf-bdd; locates the adf-bdd binary.
- oracle_terms.py
  - The term normaliser: the four standard reduction rules of the COMMA paper, clash detection and normal-form classification.
- labelled.py
  - Writes the witness labelling into the debate term (`A{IN}`, `{OUT}A`).
- evaluate.py
  - Label-guided evaluation: witness choice (skeptical/credulous), scaffold resolution by labels, normalisation, classification (value / open / exception).

Notes
- If logging needs pres.gen, import it lazily inside helper methods to avoid pres↔core cycles.
