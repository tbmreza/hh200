DB := app.db

# .PHONY: db-check db-open test e2e golden
.PHONY: db-check db-open universe

db-check:
	@if [ -f $(DB) ]; then \
		echo "Database $(DB) exists"; \
	else \
		echo "Database $(DB) does not exist"; \
	fi

db-open:
	~/go/bin/lazysql --version

# End-to-end universe: build hh200 + ocapi, start ocapi, run e2e.test.
universe:
	./scripts/e2e.sh

# # Full test run: Haskell unit tests + end-to-end golden tests.
# test:
# 	cd hh200 && stack test
# 	$(MAKE) e2e

# golden: e2e
