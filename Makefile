DB := app.db

.PHONY: db-check db-create

db-check:
	@if [ -f $(DB) ]; then \
		echo "Database $(DB) exists"; \
	else \
		echo "Database $(DB) does not exist"; \
	fi

db-create:
	@touch $(DB)
	@echo "Created $(DB)"

db-open:
	~/go/bin/lazysql --version
