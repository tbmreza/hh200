import { Database } from "bun:sqlite";

const db = new Database("metrics.db");

console.log("Creating tables...");

// Create CPU Usage table
db.query(`
  CREATE TABLE IF NOT EXISTS cpu_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp INTEGER,
    value REAL
  )
`).run();

// Create Memory Usage table
db.query(`
  CREATE TABLE IF NOT EXISTS memory_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp INTEGER,
    value REAL
  )
`).run();

console.log("Seeding data...");

const now = Date.now();
const insertCpu = db.prepare("INSERT INTO cpu_usage (timestamp, value) VALUES ($timestamp, $value)");
const insertMem = db.prepare("INSERT INTO memory_usage (timestamp, value) VALUES ($timestamp, $value)");

const oneHour = 60 * 60 * 1000;
const entries = 20;

const runTransaction = db.transaction(() => {
    for (let i = 0; i < entries; i++) {
        const time = now - (entries - i) * (oneHour / entries); // Distribute over last hour
        
        insertCpu.run({
            $timestamp: Math.floor(time),
            $value: parseFloat((Math.random() * 100).toFixed(2))
        });

        insertMem.run({
            $timestamp: Math.floor(time),
            $value: parseFloat((Math.random() * 16384).toFixed(2)) // Max 16GB
        });
    }
});

runTransaction();

console.log("Database metrics.db populated successfully.");
db.close();
