"""Write tests/slow_tests.txt, the tests `make test` leaves to `make test-all`.

    pytest tests -p no:xdist -m "slow or not slow" --junit-xml=times.xml
    python tools/list_slow_tests.py times.xml

Timings must come from a sequential run.  A test is slow if it is

- a random-document seed from 10 up, or the one costly seed of the evaluation
  sweep;
- one case of a parametrized test, taking a second or more;
- any other test, taking five seconds or more;

unless KEEP names it: the costly tests that are the default run's only way to
reach some line of core/, wrap/ or pres/.  Check KEEP against line coverage when
the list is regenerated."""

import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

OUT = Path(__file__).resolve().parent.parent / "tests" / "slow_tests.txt"
SEED = re.compile(r"test_random_documents\[(\d+)\]")
COSTLY_SEED = ("TestEvaluateFromTheSharedDebate", "2")

KEEP = {
    "tests/script_test.py::test_execute_script_runs_all",
    "tests/test_labelled_terms.py::test_labels_read_off_the_term_agree_with_the_labelling",
    "tests/test_scasp_correspondence.py::test_stable_evaluation_against_stable_models[pos-a-credulous]",
}


def node_id(testcase):
    parts = testcase.get("classname").split(".")
    modules = [p for p in parts if not p[:1].isupper()]
    classes = [p for p in parts if p[:1].isupper()]
    return "/".join(modules) + ".py::" + "::".join(classes + [testcase.get("name")])


def is_slow(testcase):
    name, seconds = testcase.get("name"), float(testcase.get("time"))
    seed = SEED.search(name)
    if seed and (int(seed[1]) >= 10
                 or (COSTLY_SEED[0] in testcase.get("classname") and seed[1] == COSTLY_SEED[1])):
        return True
    return seconds >= (1.0 if "[" in name else 5.0)


def main(junit_xml):
    cases = list(ET.parse(junit_xml).getroot().iter("testcase"))
    slow = sorted({node_id(t) for t in cases if is_slow(t)} - KEEP)
    OUT.write_text("# made by tools/list_slow_tests.py\n" + "\n".join(slow) + "\n")
    print(f"{len(slow)} of {len(cases)} tests are slow")


if __name__ == "__main__":
    main(sys.argv[1])
