const sqlite3 = require('sqlite3').verbose();
const path = require('path');
const fs = require('fs');

const dbPath = path.join(__dirname, 'metrics.db');

// Remove existing db if exists to start fresh
if (fs.existsSync(dbPath)) {
    fs.unlinkSync(dbPath);
}

const db = new sqlite3.Database(dbPath);

db.serialize(() => {
    // Table 1: requests_breakpoint
    db.run("CREATE TABLE requests_breakpoint (timestamp INTEGER, value REAL)");

    const stmt1 = db.prepare("INSERT INTO requests_breakpoint VALUES (?, ?)");
    let now = Date.now();
    for (let i = 0; i < 100; i++) {
        // Generate a timestamp for every minute in the past 100 minutes
        const ts = now - (100 - i) * 60 * 1000;
        // Generate a random value, maybe simulating requests per minute
        const val = Math.floor(Math.random() * 100) + 50;
        stmt1.run(ts, val);
    }
    stmt1.finalize();

    // Table 2: cpu_usage
    db.run("CREATE TABLE cpu_usage (timestamp INTEGER, value REAL)");
    const stmt2 = db.prepare("INSERT INTO cpu_usage VALUES (?, ?)");
    for (let i = 0; i < 100; i++) {
        const ts = now - (100 - i) * 60 * 1000;
        // CPU usage percentage
        const val = Math.random() * 100;
        stmt2.run(ts, val);
    }
    stmt2.finalize();

    console.log("Database populated successfully.");
});

db.close();
