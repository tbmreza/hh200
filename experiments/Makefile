# Centralized housekeeping + capture for the experiments workspace.
#
# Paths are anchored to this file's directory, so targets work from anywhere:
#
#   make help            list targets
#   make status          show current log/dump state
#   make clean           truncate traffic dump + remove rotated leftovers
#   make clean-traffic   truncate the root-owned traffic dump (sudo)
#   make clean-rotated   remove stale rotated/compressed dump leftovers
#   make run-closed      closed-model k6 run -> fixtures/run-closed.{dump,stdout}
#   make run-open        open-model k6 run   -> fixtures/run-open.{dump,stdout}
#   make down            stop the ocapi container

HERE       := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
OPEN_CLOSE := $(HERE)/open-close-k6
DUMP       := $(OPEN_CLOSE)/logs/ocapi.log
FIXTURES   := $(HERE)/ocapi/ocapi-core/fixtures
SUDO       ?= sudo

# Bounded readiness probe: poll :8080 for up to 10s (50 * 0.2s).
READY = ( for _ in $$(seq 1 50); do curl -fsS http://localhost:8080/ >/dev/null 2>&1 && exit 0; sleep 0.2; done; exit 1 )

.PHONY: help status clean clean-traffic clean-rotated run-closed run-open down

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

# ── run + capture ─────────────────────────────────────────────────────────
# Each run target brings up a fresh container, waits for :8080, truncates the
# dump, runs k6, then snapshots both the traffic dump (fixtures/<name>.dump)
# and the container stdout (fixtures/<name>.stdout). Re-running overwrites.

run-closed: ## Closed-model k6 (10 VUs / 8s) -> fixtures/run-closed.{dump,stdout}
	cd $(OPEN_CLOSE) && docker compose up -d --force-recreate ocapi-alpine-inst && \
	$(READY) && \
	$(SUDO) truncate -s 0 $(DUMP) && \
	cd $(OPEN_CLOSE) && mise exec -- k6 run --vus 10 --duration 8s hello.js && \
	sleep 1 && \
	cd $(OPEN_CLOSE) && docker compose logs ocapi-alpine-inst > $(FIXTURES)/run-closed.stdout && \
	cp -f $(DUMP) $(FIXTURES)/run-closed.dump

run-open: ## Open-model k6 (10 req/s / 30s) -> fixtures/run-open.{dump,stdout}
	cd $(OPEN_CLOSE) && docker compose up -d --force-recreate ocapi-alpine-inst && \
	$(READY) && \
	$(SUDO) truncate -s 0 $(DUMP) && \
	cd $(OPEN_CLOSE) && mise exec -- k6 run open-model.js && \
	sleep 1 && \
	cd $(OPEN_CLOSE) && docker compose logs ocapi-alpine-inst > $(FIXTURES)/run-open.stdout && \
	cp -f $(DUMP) $(FIXTURES)/run-open.dump

down: ## Stop the ocapi container
	cd $(OPEN_CLOSE) && docker compose down
