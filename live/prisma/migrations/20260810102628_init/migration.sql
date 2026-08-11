/*
  Warnings:

  - You are about to drop the `metrics` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropTable
PRAGMA foreign_keys=off;
DROP TABLE "metrics";
PRAGMA foreign_keys=on;

-- CreateTable
CREATE TABLE "metric_windows" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "runId" INTEGER NOT NULL,
    "endpoint" TEXT NOT NULL,
    "windowStart" DATETIME NOT NULL,
    "windowEnd" DATETIME NOT NULL,
    "count" INTEGER NOT NULL,
    "failCount" INTEGER NOT NULL,
    "minLatency" REAL NOT NULL,
    "maxLatency" REAL NOT NULL,
    "avgLatency" REAL NOT NULL,
    "rps" REAL NOT NULL,
    CONSTRAINT "metric_windows_runId_fkey" FOREIGN KEY ("runId") REFERENCES "runs" ("id") ON DELETE RESTRICT ON UPDATE CASCADE
);
