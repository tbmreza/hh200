# Dev Server Utilities

This folder contains development server utilities for testing and demonstration purposes.

## Node.js SQLite Demo Server

A simple Express.js server that serves timeseries data from an SQLite database.

### Setup and Run
1. Install dependencies:
   ```bash
   npm install
   ```

2. Populate the database (creates `metrics.db` with sample data):
   ```bash
   npm run populate
   ```

3. Start the server:
   ```bash
   npm run start
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
