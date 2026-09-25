# AC/DC demo

Six short sessions, one message each.  Each `.fspy` file has two parts,
separated by a `%stop` line: the part above it is executed when the file is
loaded, the part below it is for you to paste into the session, line by line
or as a block, from your editor.

    acdc --interactive --load tests/demo/01_arguments.fspy

then paste from below `%stop`.  Lines starting with `#` are echoed as
narration, lines starting with `%` are ignored, and a pasted block runs one
line at a time.  `load FILE` at the prompt runs another file in the same
session; `exit` leaves it.

To run a file end to end, `%stop` included, use `--script`:

    acdc --script tests/demo/03_contested.fspy

| file | shows |
|---|---|
| 01_arguments | an argument is a proof term with open sites; the document graph; `graph`, `label`, `evaluate` |
| 02_support_attack | the verbs name a debate and check it against the document; a refused support; scaffolds; the modes |
| 03_contested | grounded vs preferred labellings; skeptical vs credulous evaluation; choosing the witness |
| 04_document | a counterargument registered anywhere contests every use of its proposition |
| 05_even_loop | a cycle closes in the term by capturing the continuation; two models; semantics and mode decide |
| 06_peirce | the skeptic's offer of P begs the question; the framework sees a cycle, the term a classical proof |

Requirements: the Fellowship binary and `adf-bdd` (see the top-level README);
Graphviz for `graph ... show` (without it, a text tree is printed instead).
Every unfolded term is replayed through Fellowship, which is the slow part;
`typecheck off` disables it for a session.

Note for 06: the strict phase is what turns the sprung trap into a strict
edge. Strict in the debate means the subterm rests on nothing, that is, it
has no open site; see the note in `call-by-onus.org`.

`tests/demo/test_demo.py` runs every file end to end so the demo cannot rot.
