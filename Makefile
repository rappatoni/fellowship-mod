PYTHON ?= python3
VENV := .venv
PY := $(VENV)/bin/python
PIP := $(VENV)/bin/pip

ADF_BDD_VERSION := 0.3.0
ADF_BDD := $(VENV)/bin/adf-bdd

.PHONY: venv install reset-venv cli test lint format typecheck binlink clean adf-bdd

# Create the venv only if none exists: re-running `python3 -m venv` over a
# venv made by a different interpreter (e.g. uv's 3.12 vs Homebrew's 3.14)
# rewrites pyvenv.cfg and breaks it.
venv:
	@[ -x $(PY) ] || $(PYTHON) -m venv $(VENV)
	$(PY) -m pip install -U pip wheel

install: venv adf-bdd
	$(PIP) install -e .
	$(PIP) install -r requirements-dev.txt

# The ADF solver used as the primary labeller (see tasks.org,
# aida-adf-bdd-primary). Installed into the venv next to `acdc`, pinned.
# Requires a Rust toolchain (https://rustup.rs). No fallback exists: the
# labeller refuses to run without it.
adf-bdd:
	@command -v cargo >/dev/null 2>&1 || { echo "cargo not found: install Rust (https://rustup.rs) to build adf-bdd $(ADF_BDD_VERSION)"; exit 1; }
	cargo install adf-bdd-bin --version $(ADF_BDD_VERSION) --root $(VENV)

reset-venv:
	rm -rf $(VENV)
	$(MAKE) install

# Run the console script without activating the venv
cli: install
	$(VENV)/bin/acdc $(ARGS)

test: install
	$(VENV)/bin/pytest -q tests

lint: install
	$(VENV)/bin/ruff check core pres wrap mod tests

format: install
	$(VENV)/bin/black core pres wrap mod tests

typecheck: install
	$(VENV)/bin/mypy core pres wrap mod

# Optional: short local command
binlink:
	ln -sf .venv/bin/acdc acdc

clean:
	rm -rf $(VENV) .pytest_cache .mypy_cache build dist *.egg-info
	find . -type d -name "__pycache__" -exec rm -rf {} +
PYTHON ?= python3
