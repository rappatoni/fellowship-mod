"""Regenerate the material of paper-examples.org (lib/iris_experiment).

For every example: the snippet (copied from its literate document), the
verdict and normal form, the English check-list rendering of the evaluated
term, and, where the example has one, a decorated graph (DOT and PNG).
For the sharing example: the expanded and the shared check list of one
unfolded debate, with their sizes.

Run from the repository root with this checkout's Fellowship:

    ACDC_FSP=$PWD/wrap/fellowship/fsp .venv/bin/python lib/iris_experiment/paper/make_examples.py

Everything is written to lib/iris_experiment/paper/out/ (overwritten).
Afterwards paper-examples.org is exported, with its #+include lines expanded,
to paper-examples.md, the version GitHub can show (its Org renderer ignores
#+include). With --markdown only that export runs.
"""

from __future__ import annotations

import copy
import json
import re
import shutil
import subprocess
import sys
import tempfile
import warnings
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT))

from pres.gen import ProofTermGenerationVisitor  # noqa: E402
from tools.tangle_fspy import tangle  # noqa: E402
from wrap.cli import _decorated_text, setup_prover  # noqa: E402
from wrap.service import Service  # noqa: E402

IRIS = ROOT / "lib" / "iris_experiment"
OUT = IRIS / "paper" / "out"

# name: (script, target, evaluate options, graph?, snippet source)
# A snippet source is (org file, heading text) for the first fspy block under
# that heading, or (org file, "#name") for a named block.
EXAMPLES = {
    "E1-frag_g-credulous": ("rq3-fragments.fspy", "frag_g", {"mode": "credulous"}, True,
                            ("rq3-fragments.org", "(g) The core of the dispute")),
    "E1-frag_g-skeptical": ("rq3-fragments.fspy", "frag_g", {}, False, None),
    "E2-frag_a": ("rq3-fragments.fspy", "frag_a", {}, True,
                  ("rq3-fragments.org", "(a) Refutation only")),
    "E3-4a-delegated": ("rq4-a.fspy", "duty", {}, False, ("rq4-6-7-metanorm.org", "#duty")),
    "E3-4b-owed": ("rq4-b.fspy", "duty", {}, False, ("rq4-6-7-metanorm.org", "#duty_owed")),
    "E4-6a-complete": ("rq6-a.fspy", "duty", {}, True, ("rq4-6-7-metanorm.org", "#requirements")),
    "E4-6b-missing": ("rq6-b.fspy", "duty", {}, False, None),
    "E4-6c-doubt": ("rq6-c.fspy", "duty", {}, False, ("rq4-6-7-metanorm.org", "#doubt")),
    "E5-7a-circle": ("rq7-a.fspy", "claim", {}, True, ("rq4-6-7-metanorm.org", "#cycle")),
    "E5-7b-anchored": ("rq7-b.fspy", "claim", {}, False, ("rq4-6-7-metanorm.org", "#scope_principle")),
    "E9-6p-b-order": ("rq6p-b.fspy", "decision", {}, False, ("rq4-6-7-metanorm.org", "#orders")),
    "E9-6w-b-constructive": ("rq6w-b.fspy", "decision_c", {}, False,
                             ("rq4-6-7-metanorm.org", "#witness_constructive")),
    "E9-6k-b-classical": ("rq6k-b.fspy", "decision_k", {}, False,
                          ("rq4-6-7-metanorm.org", "#witness_classical")),
}

# The sharing example: one unfolded debate, expanded and shared.
SHARING = ("rq3-combined.fspy", "issue :Registrable[usufruct]")


def snippet(source) -> str:
    org, where = source
    lines = (IRIS / org).read_text().splitlines()
    if where.startswith("#"):
        start = next(i for i, line in enumerate(lines) if line.strip() == f"#+name: {where[1:]}")
    else:
        start = next(i for i, line in enumerate(lines) if line.startswith("*") and where in line)
    begin = next(i for i in range(start, len(lines)) if lines[i].strip().startswith("#+begin_src fspy"))
    end = next(i for i in range(begin, len(lines)) if lines[i].strip() == "#+end_src")
    return "\n".join(re.sub(r"^,(?=[*#])", "", line) for line in lines[begin + 1:end]) + "\n"


def write(name: str, text: str) -> None:
    (OUT / name).write_text(text if text.endswith("\n") else text + "\n")


def export_markdown() -> None:
    """Export paper-examples.org to paper-examples.md with Emacs's ox-md.

    The includes are expanded by the exporter; the title, which ox-md drops,
    becomes the one level-1 heading. Skipped with a message if there is no Emacs.
    """
    org, md = IRIS / "paper-examples.org", IRIS / "paper-examples.md"
    emacs = shutil.which("emacs")
    if emacs is None:
        print("no emacs on PATH: paper-examples.md not regenerated")
        return
    lisp = ("(progn (require 'ox-md)"
            " (setq org-md-headline-style 'atx org-md-toplevel-hlevel 2"
            "       org-export-with-toc nil make-backup-files nil)"
            f" (find-file {json.dumps(str(org))}) (org-export-to-file 'md {json.dumps(str(md))}))")
    subprocess.run([emacs, "-Q", "--batch", "--eval", lisp], check=True, capture_output=True)
    title = next(line.split(":", 1)[1].strip() for line in org.read_text().splitlines()
                 if line.lower().startswith("#+title:"))
    note = ("*Generated from `paper-examples.org` by `paper/make_examples.py`; "
            "edit the Org file, not this one.*")
    md.write_text(f"# {title}\n\n{note}\n\n{md.read_text().lstrip()}")
    print(f"wrote {md.relative_to(ROOT)}")


def main() -> None:
    if "--markdown" in sys.argv[1:]:
        export_markdown()
        return
    if OUT.exists():
        shutil.rmtree(OUT)
    OUT.mkdir(parents=True)
    tmp = Path(tempfile.mkdtemp())
    scripts = {p.name: p for p in tangle(sorted(IRIS.glob("*.org")), tmp)}
    prover = setup_prover()
    prover.render_files = False
    svc = Service.of(prover)
    loaded = None
    summary = []
    try:
        for name, (script, target, options, graph, source) in EXAMPLES.items():
            if script != loaded:
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore")
                    report = svc.check(scripts[script].read_text())
                assert report.ok, (script, [e.message for e in report.entries if e.status != "ok"])
                loaded = script
            ev = svc.evaluate(svc.target(target), semantics="stable", **options)
            nf = ProofTermGenerationVisitor().visit(copy.deepcopy(ev.normal_form)).pres
            mode = options.get("mode", "skeptical")
            write(f"{name}.verdict.txt", f"{target}, stable, {mode}: {ev.nf_class}\n{nf}\n")
            write(f"{name}.vanilla.txt", svc.render(target, "evaluated", "vanilla").text)
            write(f"{name}.checklist.txt", svc.render(target, "evaluated", "checklist").text)
            write(f"{name}.pruefschema.txt", svc.render(target, "evaluated", "pruefschema").text)
            # the debate before evaluation: expanded, and shared where it has
            # a shared form (an argument or issue; a debate's scope has none)
            unfolded = svc.render(target, "unfolded", "checklist").text
            write(f"{name}.unfolded.checklist.txt", unfolded)
            sizes = f"unfolded {len(unfolded.splitlines())} lines"
            if svc.target(target).kind != "debate":
                shared = svc.render(target, "shared", "checklist").text
                write(f"{name}.shared.checklist.txt", shared)
                sizes += f", shared {len(shared.splitlines())}"
            if source:
                write(f"{name}.fspy", snippet(source))
            if graph:
                result = svc.graph(svc.target(target), labels=True, semantics="stable", mode=mode)
                dot = OUT / f"{name}.dot"
                dot.write_text(result.graph.to_dot(labels=result.labels, labelling=result.labelling,
                                                   node_text=_decorated_text(prover), prose=True))
                subprocess.run(["dot", "-Tpng", str(dot), "-o", str(dot.with_suffix(".png"))], check=True)
            summary.append(f"{name}: {target} ({script}), stable {mode}: {ev.nf_class}; {sizes}")

        script, target = SHARING
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            assert svc.check(scripts[script].read_text()).ok
        expanded = svc.render(target, "unfolded", "checklist").text
        shared = svc.render(target, "shared", "checklist").text
        write("S-combined.expanded.checklist.txt", expanded)
        write("S-combined.shared.checklist.txt", shared)
        headers = len(re.findall(r"^Check \S+:$", shared, re.M))
        cites = shared.count("see check ")
        summary.append(f"S-combined: {target} ({script}): expanded {len(expanded.splitlines())} lines, "
                       f"shared {len(shared.splitlines())} lines, {headers} named sub-debates, "
                       f"{cites} citations")
    finally:
        prover.close()
        shutil.rmtree(tmp, ignore_errors=True)
    write("SUMMARY.txt", "\n".join(summary))
    print("\n".join(summary))
    export_markdown()


if __name__ == "__main__":
    main()
