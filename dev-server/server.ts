import { Database } from "bun:sqlite";

const db = new Database("metrics.db", { readonly: true });
const PORT = 3000;

console.log(`Listening on http://localhost:${PORT} ...`);

Bun.serve({
  port: PORT,
  async fetch(req) {
    const url = new URL(req.url);

    // Serve index.html
    if (url.pathname === "/" || url.pathname === "/index.html") {
      return new Response(Bun.file("public/index.html"));
    }

    // API: Get list of tables
    if (url.pathname === "/api/tables") {
      try {
        // query for all tables in sqlite_master
        const query = db.query("SELECT name FROM sqlite_master WHERE type='table' AND name != 'sqlite_sequence'");
        const tables = query.all().map((row: any) => row.name);
        return Response.json({ tables });
      } catch (err) {
        return Response.json({ error: String(err) }, { status: 500 });
      }
    }

    // API: Get data for a specific table
    if (url.pathname.startsWith("/api/data/")) {
      const tableName = url.pathname.replace("/api/data/", "");

      // basic validation to prevent SQL injection (alphanumeric + underscore only)
      if (!/^[a-zA-Z0-9_]+$/.test(tableName)) {
         return Response.json({ error: "Invalid table name" }, { status: 400 });
      }

      try {
        // Verify table exists first to avoid errors or injection side effects
        const tableExists = db.query("SELECT name FROM sqlite_master WHERE type='table' AND name = $name").get({ $name: tableName });
        
        if (!tableExists) {
            return Response.json({ error: "Table not found" }, { status: 404 });
        }

        const query = db.query(`SELECT * FROM "${tableName}" ORDER BY timestamp DESC LIMIT 100`);
        const data = query.all();
        return Response.json({ data });

      } catch (err) {
        return Response.json({ error: String(err) }, { status: 500 });
      }
    }

    return new Response("Not Found", { status: 404 });
  },
});
