const express = require('express');
const sqlite3 = require('sqlite3').verbose();
const path = require('path');
const app = express();
const port = 3000;

const dbPath = path.join(__dirname, 'metrics.db');
const db = new sqlite3.Database(dbPath, sqlite3.OPEN_READONLY, (err) => {
    if (err) {
        console.error(err.message);
    }
    console.log('Connected to the metrics database.');
});

app.use(express.static(path.join(__dirname, 'public')));

// Get list of tables
app.get('/api/tables', (req, res) => {
    db.all("SELECT name FROM sqlite_master WHERE type='table'", [], (err, rows) => {
        if (err) {
            res.status(500).json({ error: err.message });
            return;
        }
        res.json({ tables: rows.map(row => row.name) });
    });
});

// Get data for a specific table
app.get('/api/data/:table', (req, res) => {
    const tableName = req.params.table;
    // Validate table name to prevent SQL injection (simple check)
    // In a real app, you'd check against the list of known tables.
    if (!/^[a-zA-Z0-9_]+$/.test(tableName)) {
         res.status(400).json({ error: "Invalid table name" });
         return;
    }

    const limit = req.query.limit ? parseInt(req.query.limit) : 100;

    const query = `SELECT * FROM ${tableName} ORDER BY timestamp ASC LIMIT ?`;
    db.all(query, [limit], (err, rows) => {
        if (err) {
            res.status(500).json({ error: err.message });
            return;
        }
        res.json({ data: rows });
    });
});

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});
