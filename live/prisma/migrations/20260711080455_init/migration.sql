-- CreateTable
CREATE TABLE "runs" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "name" TEXT NOT NULL,
    "script_path" TEXT NOT NULL,
    "started_at" BIGINT NOT NULL,
    "ended_at" BIGINT,
    "status" TEXT NOT NULL,
    "concurrency" INTEGER NOT NULL,
    "rate_limit" REAL NOT NULL,
    "control_socket" TEXT NOT NULL
);

-- CreateTable
CREATE TABLE "requests" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "run_id" INTEGER NOT NULL,
    "seq" INTEGER NOT NULL,
    "sent_at" BIGINT NOT NULL,
    "duration_ms" REAL,
    "method" TEXT NOT NULL,
    "url" TEXT NOT NULL,
    "status_code" INTEGER,
    "error" TEXT,
    "bytes_in" INTEGER,
    "bytes_out" INTEGER,
    "worker_id" INTEGER,
    CONSTRAINT "requests_run_id_fkey" FOREIGN KEY ("run_id") REFERENCES "runs" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "request_headers" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "request_id" INTEGER NOT NULL,
    "direction" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "value" TEXT NOT NULL,
    CONSTRAINT "request_headers_request_id_fkey" FOREIGN KEY ("request_id") REFERENCES "requests" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "request_bodies" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "request_id" INTEGER NOT NULL,
    "direction" TEXT NOT NULL,
    "content" BLOB,
    "truncated" BOOLEAN NOT NULL DEFAULT false,
    CONSTRAINT "request_bodies_request_id_fkey" FOREIGN KEY ("request_id") REFERENCES "requests" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "signals" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "run_id" INTEGER NOT NULL,
    "kind" TEXT NOT NULL,
    "sent_at" BIGINT NOT NULL,
    "acked_at" BIGINT,
    CONSTRAINT "signals_run_id_fkey" FOREIGN KEY ("run_id") REFERENCES "runs" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateIndex
CREATE INDEX "requests_run_id_idx" ON "requests"("run_id");

-- CreateIndex
CREATE INDEX "request_headers_request_id_idx" ON "request_headers"("request_id");

-- CreateIndex
CREATE INDEX "request_bodies_request_id_idx" ON "request_bodies"("request_id");

-- CreateIndex
CREATE INDEX "signals_run_id_idx" ON "signals"("run_id");
