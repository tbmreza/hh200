# Centralized housekeeping + capture for the experiments workspace.
#
# Paths are anchored to this file's directory, so targets work from anywhere:
#
#   make help            list targets
#   make status          show current log/dump state
#   make clean           truncate traffic dump + remove rotated leftovers
#   make clean-traffic   truncate the root-owned traffic dump (sudo)
#   make clean-rotated   remove stale rotated/compressed dump leftovers
#   make up              (re)create the ocapi container
#   make wait            wait until ocapi answers /health on :8080
#   make run-closed      closed-model k6 run -> fixtures/run-closed.{dump,stdout}
#   make run-open        open-model k6 run   -> fixtures/run-open.{dump,stdout}
#   make down            stop the ocapi container

HERE       := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
OPEN_CLOSE := $(HERE)/open-close-k6
DUMP       := $(OPEN_CLOSE)/logs/ocapi.log
FIXTURES   := $(HERE)/ocapi/ocapi-core/fixtures
SUDO       ?= sudo

.PHONY: help status clean clean-traffic clean-rotated up wait run-closed run-open down

help: ## List available targets
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "  %-14s %s\n", $$1, $$2}'

status: ## Show current log/dump state
	@ls -l $(DUMP) 2>&1 || true
	@ls -l $(FIXTURES) 2>&1 || true

clean: clean-traffic clean-rotated ## Truncate traffic dump and remove rotated leftovers

# In-place truncation (not rm) keeps the container's O_APPEND handle valid.
clean-traffic: ## Truncate the root-owned traffic dump to zero bytes (sudo)
	$(SUDO) truncate -s 0 $(DUMP)

clean-rotated: ## Remove stale rotated/compressed dump leftovers
	rm -f $(HERE)/ocapi/rotated.log
	@find $(OPEN_CLOSE)/logs -type f \( -name 'ocapi.log.*' -o -name 'ocapi.log-*' \) -delete 2>/dev/null || true

# ── run pipeline ──────────────────────────────────────────────────────────
# `up` and `wait` are separate so they can be debugged in isolation. Each run
# target then truncates the dump, runs k6, and snapshots the traffic dump
# (fixtures/<name>.dump) + container stdout (fixtures/<name>.stdout).
# Re-running overwrites.

up: ## (re)create the ocapi container
	cd $(OPEN_CLOSE) && docker compose up -d --force-recreate ocapi-alpine-inst

wait: ## wait until ocapi answers /health on :8080
	@echo "→ waiting for ocapi /health on :8080..."
	@i=0; while [ $$i -lt 50 ]; do \
		curl -fsS http://localhost:8080/health >/dev/null 2>&1 && { echo "→ ocapi ready"; exit 0; }; \
		sleep 0.2; i=$$((i+1)); \
	done; \
	echo "✗ ocapi did not become ready on :8080 after 10s" >&2; \
	cd $(OPEN_CLOSE) && docker compose ps >&2; \
	cd $(OPEN_CLOSE) && docker compose logs --tail 20 ocapi-alpine-inst >&2; \
	exit 1

run-closed: NAME := run-closed
run-closed: K6 := --vus 10 --duration 8s hello.js
run-closed: up wait ## Closed-model k6 (10 VUs / 8s) -> fixtures/run-closed.{dump,stdout}
	@echo "→ truncating traffic dump"
	$(SUDO) truncate -s 0 $(DUMP)
	@echo "→ running: k6 run $(K6)"
	cd $(OPEN_CLOSE) && mise exec -- k6 run $(K6)
	@sleep 1
	@echo "→ capturing docker stdout -> $(NAME).stdout"
	cd $(OPEN_CLOSE) && docker compose logs ocapi-alpine-inst > $(FIXTURES)/$(NAME).stdout
	@echo "→ capturing traffic dump -> $(NAME).dump"
	cp -f $(DUMP) $(FIXTURES)/$(NAME).dump
	@echo "✓ wrote $(FIXTURES)/$(NAME).{dump,stdout}"

run-open: NAME := run-open
run-open: K6 := open-model.js
run-open: up wait ## Open-model k6 (10 req/s / 30s) -> fixtures/run-open.{dump,stdout}
	@echo "→ truncating traffic dump"
	$(SUDO) truncate -s 0 $(DUMP)
	@echo "→ running: k6 run $(K6)"
	cd $(OPEN_CLOSE) && mise exec -- k6 run $(K6)
	@sleep 1
	@echo "→ capturing docker stdout -> $(NAME).stdout"
	cd $(OPEN_CLOSE) && docker compose logs ocapi-alpine-inst > $(FIXTURES)/$(NAME).stdout
	@echo "→ capturing traffic dump -> $(NAME).dump"
	cp -f $(DUMP) $(FIXTURES)/$(NAME).dump
	@echo "✓ wrote $(FIXTURES)/$(NAME).{dump,stdout}"

down: ## Stop the ocapi container
	cd $(OPEN_CLOSE) && docker compose down
