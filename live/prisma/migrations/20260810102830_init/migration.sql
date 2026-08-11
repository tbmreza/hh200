/*
  Warnings:

  - You are about to drop the column `windowEnd` on the `metric_windows` table. All the data in the column will be lost.
  - You are about to drop the column `windowStart` on the `metric_windows` table. All the data in the column will be lost.
  - Added the required column `end` to the `metric_windows` table without a default value. This is not possible if the table is not empty.
  - Added the required column `start` to the `metric_windows` table without a default value. This is not possible if the table is not empty.

*/
-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_metric_windows" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "runId" INTEGER NOT NULL,
    "endpoint" TEXT NOT NULL,
    "start" DATETIME NOT NULL,
    "end" DATETIME NOT NULL,
    "count" INTEGER NOT NULL,
    "failCount" INTEGER NOT NULL,
    "minLatency" REAL NOT NULL,
    "maxLatency" REAL NOT NULL,
    "avgLatency" REAL NOT NULL,
    "rps" REAL NOT NULL,
    CONSTRAINT "metric_windows_runId_fkey" FOREIGN KEY ("runId") REFERENCES "runs" ("id") ON DELETE RESTRICT ON UPDATE CASCADE
);
INSERT INTO "new_metric_windows" ("avgLatency", "count", "endpoint", "failCount", "id", "maxLatency", "minLatency", "rps", "runId") SELECT "avgLatency", "count", "endpoint", "failCount", "id", "maxLatency", "minLatency", "rps", "runId" FROM "metric_windows";
DROP TABLE "metric_windows";
ALTER TABLE "new_metric_windows" RENAME TO "metric_windows";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
