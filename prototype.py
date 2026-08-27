"""
AIDA basic prototype.

Goal: take arguments, register them against the Python wrapper (which drives
the Fellowship prover), and print ACCEPTED / DEFEATED - the minimal loop the
editor's backend will need to run on every re-check.

This does NOT go through .fspy scripts or subprocess text I/O. It calls the
wrapper's own Python objects directly (ProverWrapper + Argument).

Run this from the repo root, inside WSL, after `make install` /
`make -C wrap/fellowship`:

    .venv/bin/python prototype.py

Note: this talks to the raw compiled 'fsp' binary (wrap/fellowship/fsp) via
wrap.cli.setup_prover(), the same entry point the CLI itself uses. It does
NOT go through the `acdc` console script / interactive REPL (wrap/cli.py's
own prompt is a different, human-facing thing with its own "Enter command
(or \"exit\" to quit):" prompt - that's not what ProverWrapper expects).

Status is computed with core.comp.color.DebateTermLabeller - this is the same
labeller `color`/`color-nf` use internally. green -> ACCEPTED, red ->
DEFEATED, yellow -> OPEN (unresolved goal/assumption still in the term).

Demo scenario: mirrors the lawyer/CCTV non-monotonicity example from the
handoff doc, using the metro strike example that's already in the test
suite (tests/counterarguments_and_undercut.fspy) so it's known to parse:

    arg1: EfficientMetro -> UseMetro            (assert: use the metro)
    arg2: StrikeMetro -> ~EfficientMetro         (attacks arg1's premise)
    u1  = arg2 undercuts arg1                    (arg1 should flip to DEFEATED)
    arg3: DealMetro -> ~StrikeMetro               (attacks arg2's premise)
    u2  = arg3 undercuts u1                       (arg1 should flip back to ACCEPTED)

It's non-monotonicity happening across three edits, computed by re-normalizing
the composed proof term rather than by re-running some incremental graph
algorithm. Whether that recompute-the-whole-term approach is fast enough to
back live "type and see it recolor" editing (Model 1), or whether it only
makes sense for a manual on-demand recheck (Model 3). Just now giving time and 
basic prototype.
"""

from __future__ import annotations

import time

from wrap.cli import setup_prover
from core.dc.argument import Argument
from core.comp.color import DebateTermLabeller

STATUS_MAP = {
    "green": "ACCEPTED",
    "red": "DEFEATED",
    "yellow": "OPEN",
}


def label(argument: Argument) -> str:
    """Normalize an argument and return ACCEPTED / DEFEATED / OPEN."""
    if argument.normal_body is None:
        argument.normalize()
    status = DebateTermLabeller().status_of(argument.normal_body)
    return STATUS_MAP.get(status, status.upper())


def report(argument: Argument) -> None:
    """Print the argument's name, what it's trying to prove, and 
        its final evaluation status in a clean, tabular format"""
    print(f"  {argument.name:<6} [{argument.conclusion:<15}] -> {label(argument)}")


def main() -> None:
    prover = setup_prover() # Connects Python script to the OCaml binary
    try:
        prover.send_command("lk.") 
        prover.send_command(
            "declare EfficientMetro, StrikeMetro, UseMetro, DealMetro : bool."
        )
        """ UseMetro: "We should take the metro to work."
            EfficientMetro: "The metro is currently running efficiently."
            StrikeMetro: "The metro workers are on strike."
            DealMetro: "The union and management reached a deal."
        """
        prover.send_command("declare r1: (EfficientMetro -> UseMetro).")
        prover.send_command("declare r2: (StrikeMetro -> ~EfficientMetro).")
        prover.send_command("declare r3: (DealMetro -> ~StrikeMetro).")

        arg1 = Argument(
            prover,
            name="arg1",
            conclusion="UseMetro",
            instructions=[
                "cut (EfficientMetro -> UseMetro) th",
                "axiom r1",
                "elim",
                "next",
                "axiom",
            ],
        )
        arg1.execute()
        prover.register_argument(arg1)

        arg2 = Argument(
            prover,
            name="arg2",
            conclusion="EfficientMetro",
            instructions=[
                "cut (~EfficientMetro) x",
                "cut (StrikeMetro -> ~EfficientMetro) th",
                "axiom r2",
                "elim",
                "next",
                "axiom",
                "elim",
                "axiom",
            ],
            is_anti=True,
        )
        arg2.execute()
        prover.register_argument(arg2)

        arg3 = Argument(
            prover,
            name="arg3",
            conclusion="StrikeMetro",
            instructions=[
                "cut (~StrikeMetro) x",
                "cut (DealMetro -> ~StrikeMetro) th",
                "axiom r3",
                "elim",
                "next",
                "axiom",
                "elim",
                "axiom",
            ],
            is_anti=True,
        )
        arg3.execute()
        prover.register_argument(arg3)

        print("\nStep 1 - arg1 alone:")
        report(arg1)

        print("\nStep 2 - arg2 undercuts arg1 (should flip arg1 to DEFEATED):")
        u1 = arg2.undercut(arg1, name="u1")
        prover.register_argument(u1)
        report(u1)

        print("\nStep 3 - arg3 undercuts u1 (should flip arg1 back to ACCEPTED):")
        u2 = arg3.undercut(u1, name="u2")
        prover.register_argument(u2)
        report(u2)

        # DIAGNOSTICS - u2 came out DEFEATED on first run, not the expected
        # ACCEPTED. Before assuming it's a bug in the tactic instructions
        # above, check whether this is stance-dependent: core/comp/reduce.py
        # defaults to onus_stance="skeptical", which gives exceptions
        # top reduction priority over reinstating attacks. Re-run this whole
        # script with FSP_ONUS_STANCE=credulous set in the environment and
        # compare. If credulous flips u2 to ACCEPTED, (which stance should 
        # the editor use by default?) rather than a bug to fix. If it doesn't 
        # flip either way, the tactic instructions for arg2/arg3 are the more 
        # likely culprit.
        
        #"Skeptical" means an argument is only accepted if there is absolute proof. 
        #"Credulous" means an argument is accepted as long as it's plausible and not actively destroyed.
        print("\n--- diagnostics ---")
        print(f"FSP_ONUS_STANCE = {__import__('os').getenv('FSP_ONUS_STANCE', 'skeptical (default)')}")
        print("u1 natural-language rendering:")
        print(" ", u1.render(normalized=True))
        print("u2 natural-language rendering:")
        print(" ", u2.render(normalized=True))
        full_label = DebateTermLabeller().label(u2.normal_body)
        print(f"u2 full label breakdown ({len(full_label)} nodes):")
        for nid, node_label in list(full_label.items())[:20]:
            print(f"   node {nid}: status={node_label.status} inherited={node_label.inherited_status} open_site={node_label.open_site}")

    finally:
        prover.close()


if __name__ == "__main__":
    t0 = time.time()
    main()
    print(f"\n(prover round-trip total: {time.time() - t0:.2f}s)")