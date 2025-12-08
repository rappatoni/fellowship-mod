PYTHON ?= python3
VENV := .venv
PY := $(VENV)/bin/python
PIP := $(VENV)/bin/pip

.PHONY: venv install reset-venv cli test lint format typecheck binlink clean

venv:
	$(PYTHON) -m venv $(VENV)
	$(PY) -m pip install -U pip wheel

install: venv
	$(PIP) install -e .
	$(PIP) install -r requirements-dev.txt

reset-venv:
	rm -rf $(VENV)
	$(MAKE) install

# Ejecutar el console script sin activar la venv
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

# Comando corto opcional en la raíz
binlink:
	ln -sf .venv/bin/acdc acdc

clean:
	rm -rf $(VENV) .pytest_cache .mypy_cache build dist *.egg-info
	find . -type d -name "__pycache__" -exec rm -rf {} +
PYTHON ?= python3
VENV := .venv
PY := $(VENV)/bin/python
PIP := $(VENV)/bin/pip

.PHONY: venv install reset-venv cli test lint format typecheck binlink clean

venv:
	$(PYTHON) -m venv $(VENV)
	$(PY) -m pip install -U pip wheel

install: venv
	$(PIP) install -e .
	$(PIP) install -r requirements-dev.txt

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
