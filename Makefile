DB := app.db

# .PHONY: db-check db-open test e2e golden
.PHONY: db-check db-open universe curl-tests sync-curl-tests

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

# Curl test-suite adapter: build hh200 + ocapi, start the raw replay responder,
# run the `curl/` tasty group.
curl-tests:
	./scripts/curl-tests.sh

# Refresh vendored curl test data from the pinned upstream tag.
sync-curl-tests:
	./scripts/sync-curl-tests.sh

# # Full test run: Haskell unit tests + end-to-end golden tests.
# test:
# 	cd hh200 && stack test
# 	$(MAKE) e2e

# golden: e2e
