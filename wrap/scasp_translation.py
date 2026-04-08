from dataclasses import dataclass, field
from collections import defaultdict
import re


# ---------- basic tree ----------

@dataclass
class Edge:
    color: str   # "black" or "red"
    child: "Node"


@dataclass
class Node:
    label: str
    edges: list[Edge] = field(default_factory=list)

    def add(self, color: str, child: "Node") -> "Node":
        self.edges.append(Edge(color, child))
        return child


# ---------- parsing ----------

def normalize_trace_atom(atom: str) -> str | None:
    atom = atom.strip()
    if atom == "o_nmr_check":
        return None

    if atom.startswith("not "):
        atom = atom[4:].strip()

    if atom == "o_nmr_check":
        return None

    if atom.startswith("o_"):
        rest = atom[2:]
        m = re.match(r"(.+)_\d+$", rest)
        if m:
            return m.group(1)
        return rest

    return atom


def constants_from_trace(trace_text: str) -> tuple[set[str], str | None]:
    used = set()
    root = None

    for line in trace_text.splitlines():
        line = line.strip()
        m = re.match(r"\(\d+\)\s+(.+)$", line)
        if not m:
            continue

        atom = m.group(1).strip()
        norm = normalize_trace_atom(atom)
        if norm is None:
            continue

        used.add(norm)
        if root is None:
            root = norm

    return used, root


def split_body_literals(body: str) -> list[str]:
    return [x.strip() for x in body.split(",") if x.strip()]


def parse_program(program_text: str) -> dict[str, list[list[str]]]:
    clauses_by_head = defaultdict(list)

    for raw in program_text.splitlines():
        line = raw.strip()
        if not line or line.startswith("%"):
            continue
        if not line.endswith("."):
            continue

        line = line[:-1].strip()  # remove trailing dot

        if ":-" in line:
            head, body = line.split(":-", 1)
            clauses_by_head[head.strip()].append(split_body_literals(body))
        else:
            clauses_by_head[line.strip()].append([])   # fact

    return dict(clauses_by_head)


# ---------- tree transformation ----------

def simplify_double_cont(label: str) -> str:
    # repeatedly apply cont(cont(x)) = x
    while True:
        m = re.fullmatch(r"cont\(cont\((.+)\)\)", label)
        if not m:
            return label
        label = m.group(1)


def transform_literal_for_helper(lit: str) -> str:
    lit = lit.strip()
    if lit.startswith("not "):
        x = lit[4:].strip()
        return simplify_double_cont(f"cont(cont({x}))")
    return simplify_double_cont(f"cont({lit})")


def inner_of_cont(label: str) -> str | None:
    m = re.fullmatch(r"cont\((.+)\)", label)
    return m.group(1) if m else None


def build_tree(trace_text: str, program_text: str) -> Node:
    used_constants, root_name = constants_from_trace(trace_text)
    clauses_by_head = parse_program(program_text)

    relevant_heads = {h for h in clauses_by_head if h in used_constants}
    if root_name is None:
        raise ValueError("No root found in trace.")

    root = Node(root_name)
    expanding_stack = set()

    def expand_head(head_name: str, head_node: Node):
        if head_name not in relevant_heads:
            return
        if head_name in expanding_stack:
            return

        expanding_stack.add(head_name)

        for idx, body in enumerate(clauses_by_head.get(head_name, []), start=1):
            helper = head_node.add("black", Node(f"h_{head_name}_{idx}"))

            for lit in body:
                transformed = transform_literal_for_helper(lit)
                child = helper.add("red", Node(transformed))

                # cont(X) -> X is also red
                inner = inner_of_cont(transformed)
                if inner is not None and inner in relevant_heads:
                    actual = child.add("red", Node(inner))
                    expand_head(inner, actual)

                # direct X (e.g. from not X -> cont(cont(X)) -> X)
                elif transformed in relevant_heads:
                    expand_head(transformed, child)

        expanding_stack.remove(head_name)

    expand_head(root_name, root)
    return root


# ---------- naming helpers ----------

def up(label: str) -> str:
    return label.upper()


def symbol_from_label(label: str) -> str:
    # cont(x) contributes x, everything else contributes itself
    inner = inner_of_cont(label)
    return up(inner if inner is not None else label)


def helper_arg_name(helper_label: str) -> str:
    return f"arg_{up(helper_label)}"


def continuation_arg_name(x_label: str) -> str:
    return f"arg_{up(x_label)}"


def counterarg_name(child_label: str, helper_label: str) -> str:
    inner = inner_of_cont(child_label)
    x = inner if inner is not None else child_label
    return f"carg_{up(x)}_{up(helper_label)}"


def relation_decl_name(child_label: str, helper_label: str) -> str:
    inner = inner_of_cont(child_label)
    x = inner if inner is not None else child_label
    return f"att_{up(x)}_{up(helper_label)}"


# ---------- script generation helpers ----------

def collect_constant_symbols(root: Node) -> list[str]:
    seen = set()
    order = []

    def visit(node: Node):
        sym = symbol_from_label(node.label)
        if sym not in seen:
            seen.add(sym)
            order.append(sym)

        for edge in node.edges:
            visit(edge.child)

    visit(root)
    return order


def collect_helper_nodes(root: Node) -> list[tuple[Node, Node]]:
    # returns (parent_node, helper_node)
    result = []

    def visit(node: Node):
        for edge in node.edges:
            if edge.color == "black" and edge.child.label.startswith("h_"):
                result.append((node, edge.child))
            visit(edge.child)

    visit(root)
    return result


def collect_red_helper_children(root: Node) -> list[tuple[Node, Node]]:
    # returns (helper_node, child_node) for every red helper -> child edge
    result = []

    def visit(node: Node):
        if node.label.startswith("h_"):
            for edge in node.edges:
                if edge.color == "red":
                    result.append((node, edge.child))
        for edge in node.edges:
            visit(edge.child)

    visit(root)
    return result


def collect_continuation_args(root: Node) -> list[str]:
    # for every red cont(X) -> X edge, add arg_X
    seen = set()
    order = []

    def visit(node: Node):
        for edge in node.edges:
            child = edge.child
            if edge.color == "red":
                inner = inner_of_cont(node.label)
                if inner is not None and child.label == inner:
                    name = continuation_arg_name(child.label)
                    if name not in seen:
                        seen.add(name)
                        order.append(name)
            visit(child)

    visit(root)
    return order


def helper_argument_block(helper_label: str, parent_label: str) -> str:
    H = up(helper_label)
    P = up(parent_label)
    A = helper_arg_name(helper_label)
    R = f"r_{H}"
    return "\n".join([
        f"start argument {A} {P}",
        f"cut ({H} -> {P}) h.",
        f"axiom {R}",
        "elim.",
        "next.",
        "axiom.",
        "end argument",
    ])


def continuation_argument_block(x_label: str) -> str:
    X = up(x_label)
    A = continuation_arg_name(x_label)
    return "\n".join([
        f"start argument {A} {X}",
        "next.",
        "end argument",
    ])


def counterargument_block(helper_label: str, child_label: str) -> str:
    H = up(helper_label)
    inner = inner_of_cont(child_label)
    X = up(inner if inner is not None else child_label)
    CARG = counterarg_name(child_label, helper_label)
    ATT = relation_decl_name(child_label, helper_label)

    if inner is not None:
        # child is cont(X)
        return "\n".join([
            f"start counterargument {CARG} {H}",
            f"cut ({H} - {X}) att.",
            "elim.",
            "next.",
            "axiom.",
            f"moxia {ATT}.",
            "end argument",
        ])
    else:
        # child is X
        return "\n".join([
            f"start counterargument {CARG} {H}",
            f"cut ({H} - {X}) att.",
            f"axiom {ATT}.",
            "elim s.",
            f"cut ({X}) t.",
            "next.",
            "axiom.",
            "end argument",
        ])


# ---------- relation chain generation ----------

@dataclass
class RelationBuilder:
    lines: list[str] = field(default_factory=list)
    counter: int = 1

    def add(self, kind: str, source_arg: str, target: str) -> str:
        rid = f"arg{self.counter}"
        self.counter += 1
        self.lines.append(f"{kind} {rid} {source_arg} {target}")
        return rid


def build_relation_lines(root: Node) -> list[str]:
    rb = RelationBuilder()

    def process_head_node(head_node: Node, current_target: str | None) -> str | None:
        # black helper children of this node
        black_helpers = [
            edge.child
            for edge in head_node.edges
            if edge.color == "black" and edge.child.label.startswith("h_")
        ]

        current = current_target

        # black edges => support
        if black_helpers:
            if current is None:
                current = helper_arg_name(black_helpers[0].label)
                for helper in black_helpers[1:]:
                    current = rb.add("support", helper_arg_name(helper.label), current)
            else:
                for helper in black_helpers:
                    current = rb.add("support", helper_arg_name(helper.label), current)

        # first do all direct red children of all helpers at this level
        for helper in black_helpers:
            for edge in helper.edges:
                if edge.color == "red":
                    current = rb.add("undercut", counterarg_name(edge.child.label, helper.label), current)

        # then recurse into deeper continuations / direct children
        for helper in black_helpers:
            for edge in helper.edges:
                if edge.color != "red":
                    continue

                child = edge.child
                inner = inner_of_cont(child.label)

                # cont(X) -> X  (red)  => arg_X undercuts, then recurse into X
                if inner is not None:
                    actual_red_children = [
                        e.child for e in child.edges
                        if e.color == "red" and e.child.label == inner
                    ]
                    if actual_red_children:
                        actual = actual_red_children[0]
                        current = rb.add("undercut", continuation_arg_name(actual.label), current)
                        current = process_head_node(actual, current)

                # direct X child (e.g. from not X -> X)
                elif child.edges:
                    current = process_head_node(child, current)

        return current

    process_head_node(root, None)
    return rb.lines


# ---------- main export ----------

def build_lk_script(trace_text: str, program_text: str) -> str:
    root = build_tree(trace_text, program_text)

    lines = []
    lines.append("lk.")

    # constants
    consts = collect_constant_symbols(root)
    lines.append(f"declare {', '.join(consts)} : bool.")

    # declarations for helpers
    for parent, helper in collect_helper_nodes(root):
        H = up(helper.label)
        P = up(parent.label)
        lines.append(f"declare r_{H} : ({H} -> {P}).")

    # declarations for red helper -> child edges
    for helper, child in collect_red_helper_children(root):
        H = up(helper.label)
        X = symbol_from_label(child.label)
        ATT = relation_decl_name(child.label, helper.label)
        if inner_of_cont(child.label) is not None:
            lines.append(f"deny {ATT} : ({H} - {X}).")
        else:
            lines.append(f"declare {ATT} : ({H} - {X}).")

    # helper arguments
    for parent, helper in collect_helper_nodes(root):
        lines.append("")
        lines.append(helper_argument_block(helper.label, parent.label))

    # continuation arguments
    for arg_name in collect_continuation_args(root):
        x = arg_name[len("arg_"):].lower()
        lines.append("")
        lines.append(continuation_argument_block(x))

    # counterarguments
    for helper, child in collect_red_helper_children(root):
        lines.append("")
        lines.append(counterargument_block(helper.label, child.label))

    # relation lines
    relation_lines = build_relation_lines(root)
    if relation_lines:
        lines.append("")
        lines.extend(relation_lines)

    return "\n".join(lines)


# ---------- example ----------

if __name__ == "__main__":
    trace = """
    (0) z
    (1) not b
    (1) c
    (2) not d
    (3) not o_d_d_1
    (4) not f
    (0) o_nmr_check 
    """

    program = """
    z :- not b, c.
    c :- not d.
    c :- e.
    d :- f.
    a :- e.
    z :- a.
    """

    print(build_lk_script(trace, program))

