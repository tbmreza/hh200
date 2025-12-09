# Dev Server Utilities

This folder contains development server utilities for testing and demonstration purposes.

## SQLite Demo Server

A simple web app that serves timeseries data from an SQLite database.

1. Install dependencies:
   ```bash
   bun install
   ```

2. Populate the database (creates `metrics.db` with sample data):
   ```bash
   bun run populate
   ```

3. Start the server:
   ```bash
   bun run start
   ```

### ghci
```
ghci  # after installing sqlite lib globally via cabal install --lib sqlite-simple
:set -XOverloadedStrings
import Database.SQLite.Simple
conn <- open "metrics.db"
execute conn "INSERT INTO cpu_usage (timestamp, value) VALUES (?, ?)" ("1765195198680", "80.12")
close conn
```

4. Access the dashboard at [http://localhost:3000](http://localhost:3000).

## PHP Echo Server

A simple PHP script that echoes back request details (headers, body, etc.).

### Run
To start the echo server on port 9999:

```bash
php -S localhost:9999 -f echo.php
```

You can then send requests to [http://localhost:9999](http://localhost:9999) to see them echoed back.
